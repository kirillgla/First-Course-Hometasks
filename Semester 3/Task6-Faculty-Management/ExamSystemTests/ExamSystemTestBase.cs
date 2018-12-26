using System;
using System.Linq;
using System.Threading.Tasks;
using System.Xml.Linq;
using Core;
using Core.Logging;
using JetBrains.Annotations;
using Xunit;
using Xunit.Sdk;

namespace ExamSystemTests
{
    public abstract class ExamSystemTestBase
    {
        [NotNull]
        protected abstract IExamSystem NewExamSystem([CanBeNull] ILogger logger = null);

        [Fact]
        public void TestThatAddWorksCorrectly()
        {
            var system = NewExamSystem();
            system.Add(123, 123);
            system.Add(1234, 123);
            Assert.True(system.Contains(123, 123));
            Assert.True(system.Contains(1234, 123));
        }

        [Fact]
        public void TestThatRemoveWorksCorrectly()
        {
            var system = NewExamSystem();
            system.Add(123, 123);
            system.Add(1234, 123);
            system.Remove(1234, 123);
            Assert.True(system.Contains(123, 123));
            Assert.False(system.Contains(1234, 123));
        }

        [Fact]
        public void TestConcurrentAddingWorksCorrectly()
        {
            var system = NewExamSystem();

            var firstRange = new StudentDataRange(0, 1000, 0, 10);
            var secondRange = new StudentDataRange(1000, 2000, 0, 10);
            var thirdRange = new StudentDataRange(0, 1000, 10, 20);
            var fourthRange = new StudentDataRange(1000, 2000, 10, 20);

            var firstAdder = AddStudents(system, firstRange);
            var secondAdder = AddStudents(system, secondRange);
            var thirdAdder = AddStudents(system, thirdRange);
            var fourthAdder = AddStudents(system, fourthRange);

            firstAdder.Wait();
            secondAdder.Wait();
            thirdAdder.Wait();
            fourthAdder.Wait();

            var contents = system.Contents;
            Assert.True(contents.Keys.Count == 2000);
            Assert.True(contents.Values.SelectMany(it => it).Count() == 40000);
        }

        [Fact]
        public void TestThatReadActionsAndWriteActionsNeverInterfere()
        {
            ILogger logger = new XmlLogger();
            var system = NewExamSystem(logger);

            var firstRange = new StudentDataRange(0, 100, 0, 10);
            var secondRange = new StudentDataRange(100, 200, 0, 10);

            var firstAdder = AddStudents(system, firstRange);
            var secondAdder = AddStudents(system, secondRange);

            var remover = RemoveStudents(system, firstRange);

            var firstQuerier = QueryStudents(system, firstRange);
            var secondQuerier = QueryStudents(system, secondRange);
            var thirdQuerier = QueryStudents(system, firstRange);
            var fourthQuerier = QueryStudents(system, secondRange);
            var fifthQuerier = QueryStudents(system, firstRange);
            var sixthQuerier = QueryStudents(system, secondRange);

            firstAdder.Wait();
            secondAdder.Wait();
            remover.Wait();
            firstQuerier.Wait();
            secondQuerier.Wait();
            thirdQuerier.Wait();
            fourthQuerier.Wait();
            fifthQuerier.Wait();
            sixthQuerier.Wait();

            try
            {
                // If resulting xml document is valid,
                // the algorithm is very likely to be correct
                // ReSharper disable once ReturnValueOfPureMethodIsNotUsed
                XElement.Parse(logger.Log);
            }
            catch (Exception)
            {
                throw new FalseException("Could not parse log as xml", false);
            }
        }

        static async Task AddStudents([NotNull] IExamSystem system, StudentDataRange range) => await Task.Run(() =>
        {
            for (long studentId = range.FromStudentId; studentId < range.ToStudentId; studentId += 1)
            {
                for (long courseId = range.FromCourseId; courseId < range.ToCourseId; courseId += 1)
                {
                    system.Add(studentId, courseId);
                }
            }
        });

        static async Task RemoveStudents([NotNull] IExamSystem system, StudentDataRange range) => await Task.Run(() =>
        {
            for (long studentId = range.FromStudentId; studentId < range.ToStudentId; studentId += 1)
            {
                for (long courseId = range.FromCourseId; courseId < range.ToCourseId; courseId += 1)
                {
                    system.Remove(studentId, courseId);
                }
            }
        });

        static async Task QueryStudents([NotNull] IExamSystem system, StudentDataRange range) => await Task.Run(() =>
        {
            for (long studentId = range.FromStudentId; studentId < range.ToStudentId; studentId += 1)
            {
                for (long courseId = range.FromCourseId; courseId < range.ToCourseId; courseId += 1)
                {
                    system.Contains(studentId, courseId);
                }
            }
        });
    }
}
