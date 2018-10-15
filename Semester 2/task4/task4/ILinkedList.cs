using System;
using System.Collections.Generic;

namespace Task4
{
    interface ILinkedList<T> : IEnumerable<T> where T: IComparable
    {
        uint Length { get; }
        void AddToStart<U>(U value) where U : T;
        void AddToEnd<U>(U value) where U : T;
        bool Remove(T value);
        ListElement<T> PopFirst();
        ListElement<T> PopLast();
        bool Contains(T value);
        void Print();
    }
}
