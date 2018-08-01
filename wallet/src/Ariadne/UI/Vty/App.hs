module Ariadne.UI.Vty.App
  ( initApp
  , app
  ) where

import Control.Lens (makeLensesWith, uses, (.=), (%=))
import Data.Char (toLower)
import IiExtras

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Border as B
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Focus
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Theme
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.About
import Ariadne.UI.Vty.Widget.Account
import Ariadne.UI.Vty.Widget.AddWallet
import Ariadne.UI.Vty.Widget.Help
import Ariadne.UI.Vty.Widget.Logs
import Ariadne.UI.Vty.Widget.Menu
import Ariadne.UI.Vty.Widget.Repl
import Ariadne.UI.Vty.Widget.Tree
import Ariadne.UI.Vty.Widget.Wallet

data AppScreen
  = AppScreenWallet
  | AppScreenHelp
  | AppScreenAbout
  | AppScreenLogs
  deriving (Eq)

data AppSelection
  = AppSelectionNone
  | AppSelectionWallet
  | AppSelectionAccount
  | AppSelectionAddWallet

data AppCompleted = AppCompleted | AppInProgress

data AppWidgetState =
  AppWidgetState
    { appScreen :: !AppScreen
    , appSelection :: !AppSelection
    }

data AppState =
  AppState
    { appWidget :: Widget AppState
    , appFocusRing :: B.FocusRing WidgetName
    , appNavMode :: Bool
    }

makeLensesWith postfixLFields ''AppWidgetState
makeLensesWith postfixLFields ''AppState

initApp :: UiFace -> UiLangFace -> UiHistoryFace -> AppState
initApp uiFace langFace historyFace =
  AppState
    { appWidget = appWidget
    , appFocusRing = getFocusRing appWidget
    , appNavMode = False
    }
  where
    appWidget = initWidget $ do
      setWidgetState appWidgetState
      setWidgetFocusList $ appFocusList appWidgetState
      setWidgetDrawWithFocus drawAppWidget
      setWidgetHandleKey handleAppWidgetKey
      setWidgetHandleEvent handleAppWidgetEvent

      addWidgetChild WidgetNameMenu $ initMenuWidget menuItems (widgetParentLens appScreenL)
      addWidgetChild WidgetNameTree $ initTreeWidget langFace
      addWidgetChild WidgetNameAddWallet $ initAddWalletWidget langFace
      addWidgetChild WidgetNameWallet $ initWalletWidget langFace
      addWidgetChild WidgetNameAccount $ initAccountWidget langFace
      addWidgetChild WidgetNameRepl $ initReplWidget uiFace langFace historyFace
        (widgetParentGetter $ const False)
      addWidgetChild WidgetNameHelp $ initHelpWidget langFace
      addWidgetChild WidgetNameAbout initAboutWidget
      addWidgetChild WidgetNameLogs initLogsWidget

      addWidgetEventHandler WidgetNameMenu $ \case
        WidgetEventMenuSelected -> do
          resetAppFocus
          assignWidgetLens (Lens appNavModeL) False
        _ -> return ()

    appWidgetState = AppWidgetState
      { appScreen = AppScreenWallet
      , appSelection = AppSelectionNone
      }

    menuItems = NE.fromList
      [ MenuWidgetElem AppScreenWallet "Wallet" 'w'
      , MenuWidgetElem AppScreenHelp "Help" 'h'
      , MenuWidgetElem AppScreenAbout "About" 'a'
      , MenuWidgetElem AppScreenLogs "Logs" 'l'
      ]

appFocusList :: AppWidgetState -> [WidgetNamePart]
appFocusList AppWidgetState{..} = case appScreen of
    AppScreenWallet -> [WidgetNameTree] ++ mainWidgetName ++ [WidgetNameRepl]
    AppScreenHelp -> [WidgetNameHelp, WidgetNameRepl]
    AppScreenAbout -> [WidgetNameAbout, WidgetNameRepl]
    AppScreenLogs -> [WidgetNameLogs, WidgetNameRepl]
  where
    mainWidgetName = case appSelection of
      AppSelectionNone -> []
      AppSelectionAddWallet -> [WidgetNameAddWallet]
      AppSelectionWallet -> [WidgetNameWallet]
      AppSelectionAccount -> [WidgetNameAccount]

getAppFocus :: AppState -> WidgetName
getAppFocus AppState{..} =
  if appNavMode
    then [WidgetNameMenu]
    else fromMaybe [] $ B.focusGetCurrent appFocusRing

resetAppFocus :: WidgetEventM AppWidgetState AppState ()
resetAppFocus = get >>= lift . setWidgetFocusList . appFocusList

setAppFocus :: Monad m => WidgetName -> StateT AppState m ()
setAppFocus focus = do
  current <- fromMaybe [] <$> uses appFocusRingL B.focusGetCurrent
  focus' <- uses appWidgetL $ findClosestFocus current focus
  appFocusRingL %= B.focusSetCurrent focus'

updateAppFocusRing :: StateT AppState (B.EventM WidgetName) ()
updateAppFocusRing = do
  widget <- use appWidgetL
  mcurrent <- uses appFocusRingL B.focusGetCurrent
  appFocusRingL .= getFocusRing widget
  whenJust mcurrent setAppFocus

-- The Ariadne UI view and controller a single record.
app :: B.App AppState UiEvent WidgetName
app = B.App{..} where

  appDraw :: AppState -> [B.Widget WidgetName]
  appDraw = drawApp

  appChooseCursor
    :: AppState
    -> [B.CursorLocation WidgetName]
    -> Maybe (B.CursorLocation WidgetName)
  appChooseCursor = B.focusRingCursor appFocusRing

  appHandleEvent
    :: AppState
    -> B.BrickEvent WidgetName UiEvent
    -> B.EventM WidgetName (B.Next AppState)
  appHandleEvent appState ev = do
    (completed, appState') <-
      runStateT (handleAppEvent ev <* updateAppFocusRing) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

  -- We do not use this feature of Brick.
  appStartEvent :: AppState -> B.EventM WidgetName AppState
  appStartEvent = return

  appAttrMap :: AppState -> B.AttrMap
  appAttrMap = const defaultAttrMap

drawApp :: AppState -> [B.Widget WidgetName]
drawApp appState@AppState{..} =
    [ drawWidget (getAppFocus appState) appState appWidget
    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    , B.withAttr "default" $ B.fill ' '
    ]

drawAppWidget :: WidgetName -> AppWidgetState -> WidgetDrawM AppWidgetState p (B.Widget WidgetName)
drawAppWidget focus AppWidgetState{..} = do
  widget <- ask
  let
    drawChild = drawWidgetChild focus widget
    drawChildWithFocus name char pad =
      withFocusIndicator focus [name] char pad $ drawChild name
    drawContentChild name char =
      withFocusIndicator focus [name] char 0 $ B.padLeft (B.Pad 1) $ B.withAttr "default" $ drawChild name

    drawScreen widgets =
      B.withAttr "default" $ B.vBox $
        [ drawChild WidgetNameMenu
        ] ++
        widgets ++
        [ B.joinBorders B.hBorder
        , drawChild WidgetNameRepl
        ]
    drawWalletScreen = drawScreen
        [ B.hBox
            [ drawChildWithFocus WidgetNameTree 'T' 1
            , B.joinBorders B.vBorder
            , mainWidget
            ]
        ]
      where
        mainWidget = case appSelection of
          AppSelectionAddWallet -> drawChildWithFocus WidgetNameAddWallet 'P' 1
          AppSelectionWallet -> drawChildWithFocus WidgetNameWallet 'P' 1
          AppSelectionAccount -> drawChildWithFocus WidgetNameAccount 'P' 1
          _ -> B.emptyWidget
    drawHelpScreen = drawScreen [drawContentChild WidgetNameHelp 'H']
    drawAboutScreen = drawScreen [drawContentChild WidgetNameAbout 'A']
    drawLogsScreen = drawScreen [drawContentChild WidgetNameLogs 'L']

  return $ case appScreen of
    AppScreenWallet -> drawWalletScreen
    AppScreenHelp -> drawHelpScreen
    AppScreenAbout -> drawAboutScreen
    AppScreenLogs -> drawLogsScreen

handleAppEvent
  :: B.BrickEvent WidgetName UiEvent
  -> StateT AppState (B.EventM WidgetName) AppCompleted
handleAppEvent brickEvent = do
  case brickEvent of
    B.VtyEvent vtyEv@V.EvKey{} -> do
      let
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      focus <- gets getAppFocus
      runHandler (handleWidgetEditKey editKey focus) >>= \case
        WidgetEventHandled -> return AppInProgress
        WidgetEventNotHandled ->
          runHandler (handleWidgetKey key focus) >>= \case
            WidgetEventHandled -> return AppInProgress
            WidgetEventNotHandled
              | KeyQuit <- key ->
                  return AppCompleted
              | KeyNavigation <- key -> do
                  appNavModeL %= not
                  return AppInProgress
              | KeyFocusPrev <- key -> do
                  appFocusRingL %= B.focusPrev
                  appNavModeL .= False
                  return AppInProgress
              | KeyFocusNext <- key -> do
                  appFocusRingL %= B.focusNext
                  appNavModeL .= False
                  return AppInProgress
              | Just scrollAction <- keyToScrollingAction key -> do
                  void $ runHandler $ handleWidgetScroll scrollAction focus
                  return AppInProgress
              | otherwise ->
                  return AppInProgress
    B.VtyEvent (V.EvPaste raw) -> do
      whenRight (decodeUtf8' raw) $ \pasted -> do
        focus <- gets getAppFocus
        void $ runHandler $ handleWidgetPaste pasted focus
      return AppInProgress
    B.MouseDown name button [] coords -> do
      case button of
        V.BScrollUp -> void $ runHandler $ handleWidgetScroll ScrollingLineUp name
        V.BScrollDown -> void $ runHandler $ handleWidgetScroll ScrollingLineDown name
        _ -> do
          setAppFocus name
          void $ runHandler $ handleWidgetMouseDown coords name
      return AppInProgress
    B.AppEvent (UiCommandAction UiCommandQuit) -> do
      return AppCompleted
    B.AppEvent event -> do
      runHandler $ handleWidgetEvent event
      return AppInProgress
    _ ->
      return AppInProgress
  where
    runHandler handler = do
      widget <- use appWidgetL
      (res, widget') <- runStateT handler widget
      appWidgetL .= widget'
      return res

handleAppWidgetKey
  :: KeyboardEvent
  -> WidgetEventM AppWidgetState AppState WidgetEventResult
handleAppWidgetKey key = do
    navMode <- useWidgetLens $ Lens appNavModeL
    selection <- use appSelectionL
    case key of
      KeyChar (toLower -> c)
        | navMode -> do
            whenJust (charToScreen c) $ \screen -> do
              appScreenL .= screen
              resetAppFocus
            whenJust (charToFocus selection c) $ \focus -> do
              lift . lift . setAppFocus $ [focus]
            assignWidgetLens (Lens appNavModeL) False
            return WidgetEventHandled
      _ ->
        return WidgetEventNotHandled
  where
    charToScreen = \case
      'w' -> Just AppScreenWallet
      't' -> Just AppScreenWallet
      'p' -> Just AppScreenWallet
      'h' -> Just AppScreenHelp
      'a' -> Just AppScreenAbout
      'l' -> Just AppScreenLogs
      _ -> Nothing
    charToFocus selection = \case
      't' -> Just WidgetNameTree
      'p' -> Just $ mainFocus selection
      'h' -> Just WidgetNameHelp
      'a' -> Just WidgetNameAbout
      'l' -> Just WidgetNameLogs
      'r' -> Just WidgetNameRepl
      _ -> Nothing
    mainFocus = \case
      AppSelectionNone -> WidgetNameTree
      AppSelectionAddWallet -> WidgetNameAddWallet
      AppSelectionWallet -> WidgetNameWallet
      AppSelectionAccount -> WidgetNameAccount

handleAppWidgetEvent
  :: UiEvent
  -> WidgetEventM AppWidgetState AppState ()
handleAppWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    appSelectionL .= AppSelectionAddWallet
    whenJust wuSelectionInfo $ \case
      UiSelectionWallet{} -> appSelectionL .= AppSelectionWallet
      UiSelectionAccount{} -> appSelectionL .= AppSelectionAccount
    resetAppFocus
  UiCommandAction UiCommandHelp -> do
    appScreenL .= AppScreenHelp
    resetAppFocus
  UiCommandAction UiCommandLogs -> do
    appScreenL .= AppScreenLogs
    resetAppFocus
  _ ->
    return ()