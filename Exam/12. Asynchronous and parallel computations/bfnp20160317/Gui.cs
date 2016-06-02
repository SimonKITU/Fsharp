// Experiment with Gui and async, and timeout
// sestoft@itu.dk * 2011-01-07, 2013-04-25

using System;
using System.Windows.Forms;
using System.Threading.Tasks;

class TestGui {
  public static void Main(String[] args) {
    Application.Run(new MyForm());
  }
}

class MyForm : Form {  
  public MyForm() {
    ClientSize = new System.Drawing.Size(300,200);
    FlowLayoutPanel p = new FlowLayoutPanel();
    Text = "My window";
    Button b1 = new Button(), b2 = new Button();
    p.Controls.Add(b1); 
    p.Controls.Add(b2);
    Controls.Add(p);
    int n = 34;
    b1.Text = "Next Fib";
    // Asynchronous action:
    // b1.Click += async delegate(Object sender, EventArgs e) 
    //   { 
    //     b1.Enabled = false;
    //     b1.Text = "(Computing)";
    //     Console.Write("\nComputing SlowFib({0}) = ", n); 
    //     double result = await SlowFibAsync(n++);
    //     Console.WriteLine(result); 
    //     b1.Text = "Next Fib";
    //     b1.Enabled = true;
    //   };
    // Synchronous action:
     b1.Click += delegate(Object sender, EventArgs e) 
       { 
         b1.Enabled = false;
         b1.Text = "(Computing)";
         Console.Write("\nComputing SlowFib({0}) = ", n); 
         double result = SlowFib(n++);
         Console.WriteLine(result); 
         b1.Text = "Next Fib";
         b1.Enabled = true;
      };
    b2.Text = "Click";
    int clicks = 0;
    b2.Click += delegate(Object sender, EventArgs e)     
      {
        Console.Write("[{0}] ", clicks++);
      };
    ResumeLayout(true);
  }
    
  public static double SlowFib(int n) {
    if (n < 2) 
      return 1;
    else 
      return SlowFib(n-1) + SlowFib(n-2);
  }  

  // These two versions of SlowFibAsync are equivalent I believe:

  public static Task<double> SlowFibAsync(int n) {
    return Task.Run(() => SlowFib(n));
  } 

  // public static async Task<double> SlowFibAsync(int n) {
  //   return await Task.Run(() => SlowFib(n));
  // } 

  // These two versions of SlowFibTimeout are equivalent I believe:

  public static Task<double> SlowFibTimeout1Async(int n) {
    Task<double> slowFibTask = SlowFibAsync(n);
    return Task.WhenAny(slowFibTask, Task.Delay(1000))
                 .ContinueWith<double>((Task<Task> task) => 
                   {
                     if (task.Result == slowFibTask)
                       return slowFibTask.Result;
                     else {
                       Console.WriteLine("[TIMEOUT]");
                       return -1;
                     }
                   });
  }

  public static async Task<double> SlowFibTimeout2Async(int n) {
    Task<double> slowFibTask = SlowFibAsync(n);
    if (slowFibTask == await Task.WhenAny(slowFibTask, Task.Delay(1000)))
      return slowFibTask.Result;
    else {
      Console.WriteLine("[TIMEOUT]");
      return -1;
    }
  }

  // Timeout(task, ms, fallback) creates and starts a task that
  // returns the value of the given task if produced within ms
  // milliseconds, and otherwise returns the fallback value.

  public static async Task<T> Timeout1<T>(Task<T> task, int ms, T fallback) {
    return task == await Task.WhenAny(task, Task.Delay(ms)) ? task.Result : fallback;
  }

  public static async Task<T> Timeout2<T>(Task<T> task, int ms, T fallback) {
    if (task == await Task.WhenAny(task, Task.Delay(ms)))
      return task.Result;
    else
      return fallback;
  }
}
