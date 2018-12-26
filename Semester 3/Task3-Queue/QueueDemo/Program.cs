using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Task3_Queue;

namespace QueueDemo
{
    static class Program
    {
        const int ProducerInterval = 1000;
        const int ConsumerInterval = 100;

        static void Main()
        {
            using (IProducerConsumerQueue<string> queue = new ProducerConsumerQueue())
            using (var runner = new ProducerConsumerRunner(queue))
            {
                for (int producerIndex = 0; producerIndex < 10; producerIndex += 1)
                {
                    runner.RunProducer(ProducerInterval, producerIndex).Ignore();
                }

                for (int consumerIndex = 0; consumerIndex < 10; consumerIndex += 1)
                {
                    runner.RunConsumer(ConsumerInterval, consumerIndex).Ignore();
                }

                Console.WriteLine("Press any key to stop queue");
                Console.ReadKey();
            }

            Console.WriteLine("Press any key to exit");
            Console.ReadKey();
        }

        static void Ignore([NotNull] this Task task)
        {
            if (task == null)
            {
                throw new ArgumentNullException(nameof(task));
            }
        }
    }
}
