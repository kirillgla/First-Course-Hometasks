using System;
using System.Collections;
using System.Collections.Generic;

namespace Task4
{
    class LinkedList<T> : ILinkedList<T> where T: IComparable
    {
        private ListElement<T> first;
        private ListElement<T> last;
        private uint length;

        public T First { get => first.Value; }
        public T Last { get => last.Value; }
        public uint Length { get => length; }

        public LinkedList()
        {
            first = null;
            last = null;
            length = 0;
        }

        public void AddToStart<U>(U value) where U : T
        {
            var element = new ListElement<T>(value);

            if (length == 0)
            {
                first = element;
                last = element;
                length = 1;
                return;
            }

            first.Previous = element;
            first = element;
            length++;
        }

        public void AddToEnd<U>(U value) where U : T
        {
            var element = new ListElement<T>(value);

            if (length == 0)
            {
                first = element;
                last = element;
                length = 1;
                return;
            }

            last.Next = element;
            last = element;
            length++;
        }

        // current is assumed to be non-null
        // list.Length is assumed to be at least 2
        private bool RemoveAfter(ListElement<T> current, T value)
        {
            if (current.Value.CompareTo(value) == 0)
            {
                var previous = current.Previous;
                var next = current.Next;

                current.Previous = null;
                current.Next = null;

                if (previous == null)
                    first = next;

                if (next == null)
                    last = previous;

                if (previous != null && next != null)
                    previous.Next = next;

                return true;
            }

            if (current.Next == null)
                return false;

            return RemoveAfter(current.Next, value);
        }

        public bool Remove(T value)
        {
            if (length == 0)
                return false;

            if (length == 1)
            {
                if (first.Value.CompareTo(value) == 0)
                {
                    first = null;
                    last = null;
                    length = 0;
                    return true;
                }
                return false;
            }
            return RemoveAfter(first, value);
        }

        public ListElement<T> PopFirst()
        {
            if (length == 0)
                throw new EmptyListException();

            var tmp = first;

            if (length == 1)
            {
                first = null;
                last = null;
                length = 0;
                return tmp;
            }

            first = first.Next;
            tmp.Next = null;
            return tmp;
        }

        public ListElement<T> PopLast()
        {
            if (length == 0)
                throw new EmptyListException();

            var tmp = last;

            if (length == 1)
            {
                first = null;
                last = null;
                length = 0;
                return tmp;
            }

            last = last.Previous;
            tmp.Previous = null;
            return tmp;
        }

        public bool Empty()
        {
            return length == 0;
        }

        public void Clear()
        {
            first = null;
            last = null;
            length = 0;
        }

        public bool Contains(T value)
        {
            var element = first;

            if (element == null)
                return false;

            while (true)
            {
                if (element.Value.CompareTo(value) == 0)
                    return true;

                if (element.Next == null)
                    return false;

                element = element.Next;
            }
        }

        public void Print()
        {
            var element = first;
            if (element == null)
            {
                Console.WriteLine("[]");
                return;
            }
            Console.Write("[");
            while (true)
            {
                Console.Write(element.Value);
                if (element.Next != null)
                {
                    Console.Write(", ");
                    element = element.Next;
                }
                else break;
            }
            Console.WriteLine("]");
        }

        public IEnumerator<T> GetEnumerator()
        {
            var element = first;
            while (element != null)
            {
                yield return element.Value;
                element = element.Next;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
