namespace ExamSystemDemo
{
    public sealed class ProfileResult
    {
        public int[] AddRequests { get; }
        public int[] RemoveRequests { get; }
        public int[] ContainsRequests { get; }

        public ProfileResult(int addRequests, int removeRequests, int containsRequests)
        {
            AddRequests = new int[addRequests];
            RemoveRequests = new int[removeRequests];
            ContainsRequests = new int[containsRequests];
        }

        public ProfileResult(int[] addRequests, int[] removeRequests, int[] containsRequests)
        {
            AddRequests = addRequests;
            RemoveRequests = removeRequests;
            ContainsRequests = containsRequests;
        }
    }
}
