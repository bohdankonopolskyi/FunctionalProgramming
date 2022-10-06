// See https://aka.ms/new-console-template for more information
var array = Singleton("item");
Console.WriteLine("Singleton");
Print(array);
Console.WriteLine($"Null: {Null(array)}");
Console.WriteLine("Snoc");
array = Snoc(array, "item2");
array = Snoc(array, "item3");
Print(array);
int length = Length(array);
Console.WriteLine($"Length: {length}");

T[] Singleton<T>(T Tvalue) => new[] { Tvalue };
bool Null<T>(T[] array) => array is null;

T[] Snoc<T>(T[] array, T item)
{
    try
    {
        int length = array.Length;
        Array.Resize(ref array, length+1);
        array[length] = item;
    }
    catch (Exception e)
    {
        Console.WriteLine(e);
        throw;
    }
    return array;
}

int Length<T>(T[] array)
{
    int length = 0;
    if(!Null(array))
        foreach (var item in array)
        {
            length += 1;
        }

    return length;
}

void Print<T>(T[] array)
{
    foreach (var item in array)
    {
        Console.WriteLine(item.ToString());
    }
}
