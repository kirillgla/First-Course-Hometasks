using System.Collections.Generic;

namespace Math
{
    /// <summary>
    /// Class encpsulating information about curve on plane
    /// </summary>
    public abstract class CurveInfo
    {
        public abstract string Name { get; }

        public IEnumerable<ConnectivityComponent> GetConnectivityComponents(Region region)
        {
            double step = region.Width / 512;

            const double minStep = double.Epsilon * 16;

            if (step < minStep)
            {
                step = minStep;
            }

            return UnsafeGetConnectivityComponents(region, step);
        }

        internal abstract IEnumerable<ConnectivityComponent>
            UnsafeGetConnectivityComponents(Region region, double step);
    }
}
