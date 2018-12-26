namespace Core
{
    public struct StudentDataRange
    {
        public readonly long FromStudentId;
        public readonly long ToStudentId;
        public readonly long FromCourseId;
        public readonly long ToCourseId;

        public StudentDataRange(long fromStudentId, long toStudentId, long fromCourseId, long toCourseId)
        {
            FromStudentId = fromStudentId;
            ToStudentId = toStudentId;
            FromCourseId = fromCourseId;
            ToCourseId = toCourseId;
        }
    }

}
