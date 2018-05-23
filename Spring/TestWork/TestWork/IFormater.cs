namespace TestWork
{
    public interface IFormater<in T>
    {
        FormatMode Mode { get; }
        string Format(T t);
    }
}
