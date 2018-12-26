using Core;
using Core.Logging;
using Core.Synchronization;
using Core.Synchronization.ReadWrite;

namespace ExamSystemTests
{
    public sealed class WriterPriorityTests : ExamSystemTestBase
    {
        protected override IExamSystem NewExamSystem(ILogger logger = null) =>
            new GlobalLockExamSystem(new WriterPriorityLock(), logger ?? new SilentLogger());
    }
}
