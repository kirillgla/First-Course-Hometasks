using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using Task9.Tokenization;

namespace Task9.Parsing
{
    public sealed class SyntaxTreeNode
    {
        [NotNull]
        [ItemNotNull]
        public IList<SyntaxTreeNode> Children { get; }

        [NotNull]
        public Token Token { get; }

        public SyntaxTreeNode([NotNull] Token token, [NotNull] [ItemNotNull] params SyntaxTreeNode[] children)
        {
            if (token.TokenType == TokenType.Operator && Operator.Forward.Equals(token.Value))
            {
                throw new InvalidSyntaxException("Cannot create tree node of '|' operator");
            }

            Token = token;

            Children = new List<SyntaxTreeNode>(children);
        }

        public override string ToString()
        {
            return ToString(0);
        }

        string ToString(int indents)
        {
            string result = $"{new string(' ', indents * 4)}{Token}{Environment.NewLine}";
            // I know this operation produces a lot of garbage.
            // I don't care.
            return Children.Aggregate(result, (current, node) => current + node.ToString(indents + 1));
        }

        [CanBeNull]
        internal object Execute(Context context, ExecutionType executionType)
        {
            var arguments = from child in Children select child.Execute(context, ExecutionType.Argument);
            return Token.Execute(arguments, context, executionType);
        }
    }
}
