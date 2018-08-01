-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , oneGeek
       , oneGeekSK
       ) where

import Data.Time.Clock (UTCTime)

import qualified Dscp.Core as Core
import Dscp.Core.Arbitrary (studentEx, studentSKEx)
import Dscp.Crypto
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Logic (makeSubmissionVerified)
import qualified Dscp.Educator.Web.Student.Queries as Queries

import Dscp.Educator.Web.Student.Types

type StudentApiWorkMode m =
    ( Queries.MonadStudentAPIQuery m
    )

studentApiHandlers
    :: forall m. StudentApiWorkMode m
    => StudentApiHandlers m
studentApiHandlers =
    StudentApiEndpoints
    { sGetCourses = getCourses
    , sGetCourse = getCourse
    , sGetAssignments = getAssignments
    , sGetAssignment = getAssignment
    , sGetSubmissions = getSubmissions
    , sGetSubmission = getSubmission
    , sMakeSubmission = makeSubmission
    , sDeleteSubmission = deleteSubmission
    , sGetProofs = getProofs
    }

-- TODO [DSCP-141]: remove these two
oneGeek :: Student
oneGeek = studentEx

oneGeekSK :: SecretKey
oneGeekSK = studentSKEx

getCourses
    :: StudentApiWorkMode m
    => Maybe IsEnrolled -> m [Course]
getCourses isEnrolled =
    sqlTransaction $ Queries.getCourses oneGeek isEnrolled

getCourse
    :: StudentApiWorkMode m
    => Core.Course -> m Course
getCourse courseId =
    Queries.getCourse oneGeek courseId

getAssignments
    :: StudentApiWorkMode m
    => Maybe Core.Course -> Maybe Core.DocumentType -> Maybe IsFinal
    -> m [Assignment]
getAssignments mcourseId mdocType mIsFinal =
    sqlTransaction $ Queries.getAssignments oneGeek mcourseId mdocType mIsFinal

getAssignment
    :: StudentApiWorkMode m
    => Hash Core.Assignment -> m Assignment
getAssignment assignH =
    sqlTransaction $ Queries.getAssignment oneGeek assignH

getSubmissions
    :: StudentApiWorkMode m
    => Maybe Core.Course -> Maybe (Hash Core.Assignment) -> Maybe Core.DocumentType
    -> m [Submission]
getSubmissions mcourseId massignH mdocType =
    sqlTransaction $ Queries.getSubmissions oneGeek mcourseId massignH mdocType

getSubmission
    :: StudentApiWorkMode m
    => Hash Core.Submission
    -> m Submission
getSubmission submissionH =
    sqlTransaction $ Queries.getSubmission oneGeek submissionH

makeSubmission
    :: StudentApiWorkMode m
    => Core.SignedSubmission -> m Submission
makeSubmission signedSubmission =
    makeSubmissionVerified oneGeek signedSubmission

deleteSubmission
    :: StudentApiWorkMode m
    => Hash Core.Submission
    -> m ()
deleteSubmission submissionH =
    sqlTransaction $ Queries.deleteSubmission oneGeek submissionH

getProofs
    :: StudentApiWorkMode m
    => Maybe UTCTime -> m [BlkProof]
getProofs sinceF =
    sqlTransaction $ Queries.getProofs oneGeek sinceF