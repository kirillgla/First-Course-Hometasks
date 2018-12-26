using System;
using JetBrains.Annotations;

namespace Core
{
    public struct CreditData : IComparable<CreditData>, IComparable, IEquatable<CreditData>
    {
        public long StudentId { get; }
        public long CourseId { get; }

        public CreditData(long studentId, long courseId)
        {
            StudentId = studentId;
            CourseId = courseId;
        }

        public int CompareTo(CreditData other)
        {
            int studentIdComparison = StudentId.CompareTo(other.StudentId);
            return studentIdComparison != 0 ? studentIdComparison : CourseId.CompareTo(other.CourseId);
        }

        public int CompareTo([CanBeNull] object obj)
        {
            if (ReferenceEquals(null, obj))
            {
                return 1;
            }

            return obj is CreditData other
                ? CompareTo(other)
                : throw new ArgumentException($"Object must be of type {nameof(CreditData)}");
        }

        public static CreditData Min => new CreditData(long.MinValue, long.MinValue);
        public static CreditData Max => new CreditData(long.MaxValue, long.MaxValue);

        public bool Equals(CreditData other) => StudentId == other.StudentId && CourseId == other.CourseId;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj))
            {
                return false;
            }

            return obj is CreditData other && Equals(other);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (StudentId.GetHashCode() * 397) ^ CourseId.GetHashCode();
            }
        }

        public static bool operator ==(CreditData a, CreditData b) => a.Equals(b);

        public static bool operator !=(CreditData a, CreditData b) => !(a == b);

        public static bool operator >(CreditData a, CreditData b) => a.CompareTo(b) > 0;

        public static bool operator <(CreditData a, CreditData b) => a.CompareTo(b) < 0;

        public static bool operator >=(CreditData a, CreditData b) => a.CompareTo(b) >= 0;

        public static bool operator <=(CreditData a, CreditData b) => a.CompareTo(b) <= 0;

        public override string ToString() => $"{nameof(StudentId)}: {StudentId}, {nameof(CourseId)}: {CourseId}";
    }
}
