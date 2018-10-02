{-# LANGUAGE StrictData #-}

-- | Types specific to student API.

module Dscp.Educator.Web.Student.Types
    (
      -- * Flags
      IsEnrolled (..)

      -- * Requests
    , NewSubmission (..)
    , nsOwner

      -- * Responses
    , CourseStudentInfo (..)
    , AssignmentStudentInfo (..)
    , SubmissionStudentInfo (..)
    , saDocumentType

      -- * Conversions
    , studentLiftAssignment
    , studentLiftSubmission
    , signedSubmissionToRequest
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant (FromHttpApiData)
import Fmt (build, (+|), (|+), blockListF)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Servant (ForResponseLog (..), buildShortResponseList)

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled { unIsEnrolled :: Bool }
    deriving (Eq, Show)

data NewSubmission = NewSubmission
    { nsAssignmentHash :: (Hash Assignment)
    , nsContentsHash   :: (Hash Raw)
    , nsWitness        :: SubmissionWitness
    } deriving (Show, Eq, Generic)

nsOwner :: NewSubmission -> Id Student
nsOwner = mkAddr . _swKey . nsWitness

data CourseStudentInfo = CourseStudentInfo
    { ciId         :: Course
    , ciDesc       :: Text
    , ciSubjects   :: [Subject]
    , ciIsEnrolled :: Bool
    , ciIsFinished :: Bool
    } deriving (Show, Eq, Generic)

data AssignmentStudentInfo = AssignmentStudentInfo
    { aiHash           :: (Hash Assignment)
    , aiCourseId       :: Course
    , aiContentsHash   :: (Hash Raw)
    , aiIsFinal        :: IsFinal
    , aiDesc           :: Text
    , aiLastSubmission :: (Maybe SubmissionStudentInfo)
    } deriving (Show, Eq, Generic)

data SubmissionStudentInfo = SubmissionStudentInfo
    { siHash           :: (Hash Submission)
    , siContentsHash   :: (Hash Raw)
    , siAssignmentHash :: (Hash Assignment)
    , siGrade          :: (Maybe GradeInfo)
    } deriving (Show, Eq, Generic)

saDocumentType :: AssignmentStudentInfo -> DocumentType
saDocumentType = documentType . aiContentsHash

---------------------------------------------------------------------------
-- ResponseCase instances
---------------------------------------------------------------------------

type instance ResponseCase 'StudentTag Course     = CourseStudentInfo
type instance ResponseCase 'StudentTag Assignment = AssignmentStudentInfo
type instance ResponseCase 'StudentTag Submission = SubmissionStudentInfo

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

studentLiftAssignment
    :: Assignment -> Maybe SubmissionStudentInfo -> AssignmentStudentInfo
studentLiftAssignment a lastSubmission =
    AssignmentStudentInfo
    { aiHash = hash a
    , aiCourseId = _aCourseId a
    , aiContentsHash = _aContentsHash a
    , aiIsFinal = _aType a ^. assignmentTypeRaw
    , aiDesc = _aDesc a
    , aiLastSubmission = lastSubmission
    }

studentLiftSubmission :: Submission -> Maybe GradeInfo -> SubmissionStudentInfo
studentLiftSubmission s siGrade =
    SubmissionStudentInfo
    { siHash = hash s
    , siContentsHash = _sContentsHash s
    , siAssignmentHash = _sAssignmentHash s
    , ..
    }

signedSubmissionToRequest :: SignedSubmission -> NewSubmission
signedSubmissionToRequest sigSub =
    let submission = _ssSubmission sigSub
    in NewSubmission
        { nsAssignmentHash = _sAssignmentHash submission
        , nsContentsHash = _sContentsHash submission
        , nsWitness = _ssWitness sigSub
        }

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable (IsEnrolled) where
    build (IsEnrolled{..}) =
      "{ is enrolled = " +| unIsEnrolled |+
      " }"

instance Buildable (NewSubmission) where
    build (NewSubmission{..}) =
      "{ assignment hash = " +| nsAssignmentHash |+
      ", content hash = " +| nsContentsHash |+
      " }"

instance Buildable (CourseStudentInfo) where
    build (CourseStudentInfo{..}) =
      "{ course id = " +| ciId |+
      ", description = " +| ciDesc |+
      ", subjects = " +| blockListF ciSubjects |+
      ", is enrolled =" +| ciIsEnrolled |+
      ", is finished =" +|ciIsFinished |+
      " }"

instance Buildable (AssignmentStudentInfo) where
    build (AssignmentStudentInfo{..}) =
      "{ course id = " +| aiCourseId |+
      ", assignment hash = " +| aiHash |+
      ", description = " +| aiDesc |+
      " }"

instance Buildable (SubmissionStudentInfo) where
    build (SubmissionStudentInfo{..}) =
      "{ submission hash = " +| siHash |+
      ", content hash = " +| siContentsHash |+
      ", assignment hash = " +| siAssignmentHash |+
      " }"

instance Buildable (ForResponseLog CourseStudentInfo) where
    build (ForResponseLog CourseStudentInfo{..}) =
      "{ course id = " +| ciId |+
      " }"

instance Buildable (ForResponseLog AssignmentStudentInfo) where
    build (ForResponseLog AssignmentStudentInfo{..}) =
      "{ hash = " +| aiHash |+
      " }"

instance Buildable (ForResponseLog SubmissionStudentInfo) where
    build (ForResponseLog SubmissionStudentInfo{..}) =
      "{ hash = " +| siHash |+
      " }"

instance Buildable (ForResponseLog [CourseStudentInfo]) where
    build = buildShortResponseList

instance Buildable (ForResponseLog [AssignmentStudentInfo]) where
    build = buildShortResponseList

instance Buildable (ForResponseLog [SubmissionStudentInfo]) where
    build = buildShortResponseList

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewSubmission
deriveJSON defaultOptions ''CourseStudentInfo
deriveJSON defaultOptions ''AssignmentStudentInfo
deriveJSON defaultOptions ''SubmissionStudentInfo

---------------------------------------------------------------------------
-- HttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData IsEnrolled
