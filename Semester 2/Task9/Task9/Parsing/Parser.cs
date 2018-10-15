using System.Collections.Generic;
using JetBrains.Annotations;
using Task9.Tokenization;

namespace Task9.Parsing
{
    public static class Parser
    {
        [Pure]
        public static SyntaxTree Parse(IEnumerable<Token> tokens)
        {
            SyntaxTreeNode root = null;
            SyntaxTreeNode forwardedNode = null;

            foreach (var token in tokens)
            {
                if (root == null)
                {
                    root = new SyntaxTreeNode(token);
                    if (forwardedNode != null)
                    {
                        root.Children.Add(forwardedNode);
                    }
                    continue;
                }

                switch (token.TokenType)
                {
                    case TokenType.Operator when Operator.Forward.Equals(token.Value):
                        forwardedNode = root;
                        root = null;
                        continue;
                    case TokenType.Operator:
                        var tmp = root;
                        root = new SyntaxTreeNode(token);
                        root.Children.Add(tmp);
                        continue;
                    default:
                        root.Children.Add(new SyntaxTreeNode(token));
                        continue;
                }
            }

            return new SyntaxTree(root);
        }
    }
}
