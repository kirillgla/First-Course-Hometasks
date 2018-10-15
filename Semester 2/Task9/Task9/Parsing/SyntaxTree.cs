namespace Task9.Parsing
{
    public class SyntaxTree
    {
        SyntaxTreeNode First { get; }

        public SyntaxTree(SyntaxTreeNode first)
        {
            First = first;
        }

        public object Execute(Context context)
        {
            return First.Execute(context, ExecutionType.Command);
        }

        public override string ToString()
        {
            return First.ToString();
        }
    }
}
