namespace Tools.Model
{
    public struct ImageAddress
    {
        public ImageAddress(int index, int jndex, ColourPart colourPart)
        {
            Index = index;
            Jndex = jndex;
            ColourPart = colourPart;
        }

        public int Index { get; }
        public int Jndex { get; }
        public ColourPart ColourPart { get; }
    }
}
