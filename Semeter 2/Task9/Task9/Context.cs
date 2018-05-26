using System.Collections.Generic;

namespace Task9
{
    public class Context
    {
        public string Path { get; }

        readonly IDictionary<string, object> variables;

        public object GetVariable(string key) => variables[key];

        public void SetVariable(string key, object value) => variables[key] = value;

        public Context(string path)
        {
            Path = path;
            variables = new Dictionary<string, object>();
        }
    }
}
