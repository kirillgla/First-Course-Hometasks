using System;
using System.Collections.Generic;
using System.Threading;
using WeakHashTable;
using Xunit;

namespace WeakHashTableTests
{
    class MyCustomClass
    {
        string Name { get; }

        public MyCustomClass(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }

            if (obj.GetType() != GetType())
            {
                return false;
            }

            var other = (MyCustomClass) obj;

            return Name == other.Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
    }
    
    public class Tests
    {
        [Theory]
        [InlineData(1000)]
        [InlineData(2000)]
        [InlineData(5000)]
        public void TestWeakHashTableHasValidTimeToStore(int duration)
        {
            WeakHashTable<string, string> table = new WeakHashTable<string, string>(duration);
            
            Assert.Equal(duration, table.TimeToStore);
        }

        [Fact]
        public void TestEmptyWeakHashTableDoesNotContainKeys()
        {
            // ReSharper disable once CollectionNeverUpdated.Local
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            Assert.False(dictionary.ContainsKey("John"));
            Assert.False(dictionary.ContainsKey(string.Empty));
        }

        [Fact]
        public void TestEmptyWeakHashTableDoesNotRemoveValues()
        {
            // ReSharper disable once CollectionNeverUpdated.Local
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            Assert.False(dictionary.Remove("John"));
            Assert.False(dictionary.Remove(string.Empty));
        }

        [Fact]
        public void TestEmptyWeakHashTableDoesNotReturnValues()
        {
            // ReSharper disable once CollectionNeverUpdated.Local
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            Assert.False(dictionary.TryGetValue("John", out string _));
            Assert.False(dictionary.TryGetValue(string.Empty, out string _));
        }

        [Fact]
        public void TestEmptyWeakHashTableThrowsExceptionOnKeyNotFoud()
        {
            // ReSharper disable once CollectionNeverUpdated.Local
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);

            Assert.Throws<KeyNotFoundException>(() => dictionary.GetValue("Kirill"));
            Assert.Throws<KeyNotFoundException>(() => dictionary.GetValue("Hello"));
        }

        [Fact]
        public void TestWeakHashTavleAddsValuesCorrectly()
        {
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            dictionary.SetValue("Hello", "World");
            
            Assert.Equal("World",  dictionary.GetValue("Hello"));
        }
        
        [Fact]
        public void TestWeakHashTableReplacesKeysOverShortTime()
        {
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            dictionary.SetValue("Hello", "World");
            dictionary.SetValue("Hello", "Kirill");

            Assert.Equal("Kirill", dictionary.GetValue("Hello"));
        }

        [Fact]
        public void TestWeakHashTableReplacesKeysOverLongTime()
        {
            ISimpleHashTable<string, string> dictionary = new WeakHashTable<string, string>(1000);
            
            dictionary.SetValue("Hello", "World");
            
            Thread.Sleep(2000);
            
            dictionary.SetValue("Hello", "Kirill");

            Assert.Equal("Kirill", dictionary.GetValue("Hello"));
        }
        
        [Fact]
        public void TestWeakHashTableOnValuesAddedOverShortPeriodOfTime()
        {
            ISimpleHashTable<string, MyCustomClass> dictionary = new WeakHashTable<string, MyCustomClass>(1000);

            dictionary.SetValue("Kirill", new MyCustomClass("Glazyrin"));
            dictionary.SetValue("Hello", new MyCustomClass("World"));
            
            Assert.True(dictionary.ContainsKey("Kirill"));
            Assert.True(dictionary.ContainsKey("Hello"));

            Assert.Equal(new MyCustomClass("Glazyrin"), dictionary.GetValue("Kirill"));
            Assert.Equal(new MyCustomClass("World"), dictionary.GetValue("Hello"));
        }
        
        [Fact]
        public void TestWeakHashTableOnValuesAddedOverIntermediatePeriodOfTime()
        {
            ISimpleHashTable<string, MyCustomClass> dictionary = new WeakHashTable<string, MyCustomClass>(1000);
            
            dictionary.SetValue("Kirill", new MyCustomClass("Glazyrin"));
            dictionary.SetValue("Hello", new MyCustomClass("World"));

            Thread.Sleep(500);
            GC.Collect();
            
            Assert.True(dictionary.ContainsKey("Kirill"));
            Assert.True(dictionary.ContainsKey("Hello"));

            Assert.Equal(new MyCustomClass("Glazyrin"), dictionary.GetValue("Kirill"));
            Assert.Equal(new MyCustomClass("World"), dictionary.GetValue("Hello"));
        }

        [Fact]
        public void TestWeakHashTableOnValuesAddedOverLongPeriodOfTime()
        {
            ISimpleHashTable<string, MyCustomClass> dictionary = new WeakHashTable<string, MyCustomClass>(1000);
            
            dictionary.SetValue("Kirill", new MyCustomClass("Glazyrin"));
            dictionary.SetValue("Hello", new MyCustomClass("World"));

            Thread.Sleep(2000);
            GC.Collect();
            
            Assert.False(dictionary.ContainsKey("Kirill"));
            Assert.False(dictionary.ContainsKey("Hello"));
        }
    }
}
