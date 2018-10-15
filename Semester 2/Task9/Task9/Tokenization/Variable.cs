using System.Collections.Generic;
using JetBrains.Annotations;

namespace Task9.Tokenization
{
    public class Variable
    {
        public string Name { get; }

        readonly Context context;

        public Variable(string name, Context context)
        {
            Name = name;
            this.context = context;
        }

        public object Compute()
        {
            try
            {
                return context.GetVariable(Name);
            }
            catch (KeyNotFoundException)
            {
                throw new KeyNotFoundException(Name);
            }
        }

        public void Set(object value)
        {
            context.SetVariable(Name, value);
        }
    }
}
