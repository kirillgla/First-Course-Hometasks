using System;
using System.Threading.Tasks;

namespace Tools.Functional
{
    // I want nullability to be explicit and compiler enforced,
    // as opposed to Jetbrains annotations
    // ...
    // (a few days later)
    // Well, this might not have been the best decision ever,
    // but I don't want to rewrite the existing code
    public abstract class Maybe<T>
    {
        public abstract U Destruct<U>(U defaultValue, Func<T, U> f);
        public abstract Maybe<U> Fmap<U>(Func<T, U> f);
    }

    public sealed class Just<T> : Maybe<T>
    {
        public T Value { get; }
        public Just(T value) => Value = value;
        public override U Destruct<U>(U defaultValue, Func<T, U> f) => f(Value);
        public override Maybe<U> Fmap<U>(Func<T, U> f) => new Just<U>(f(Value));
    }

    public sealed class Nothing<T> : Maybe<T>
    {
        public override U Destruct<U>(U defaultValue, Func<T, U> f) => defaultValue;
        public override Maybe<U> Fmap<U>(Func<T, U> f) => new Nothing<U>();
    }
}
