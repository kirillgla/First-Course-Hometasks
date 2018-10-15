using System;
using System.Collections.Generic;

namespace Math
{
    public class ConnectivityComponent
    {
        public IEnumerable<Point> Dots { get; }
        
        internal ConnectivityComponent(
            IEnumerable<Point> dots)
        {
            Dots = dots;
        }
    }
}
