#include "Vtb_rot_top.h"
#include "verilated.h"
#include <verilated_vcd_c.h>
#include "stdlib.h"
#include <iostream>

using namespace std;

int main(int argc, char **argv, char **env) {
    // Initialize Verilator
    Verilated::commandArgs(argc, argv);

    Vtb_rot_top* rot_top = new Vtb_rot_top;

    Verilated::traceEverOn(true);
    VerilatedVcdC *m_trace  = new VerilatedVcdC;
    
    // Instantiate the rot_top module
    rot_top->trace(m_trace, 5);
    m_trace->open("wave.vcd");

    // Initialize the input signals
    rot_top->clk_i = 0;
    rot_top->clk_edn_i = 0;
    rot_top->rst_ni = 0;
    rot_top->rst_shadowed_ni = 0;
    rot_top->rst_edn_ni = 0;

    // Simulation loop
    for (int i = 0; i < 5000; ++i) {
        // Toggle the clock
        rot_top->clk_i = !rot_top->clk_i;
        rot_top->clk_edn_i = !rot_top->clk_edn_i;

        // Apply reset
        if (i == 10) {
            rot_top->rst_ni = 1;
            rot_top->rst_shadowed_ni = 1;
            rot_top->rst_edn_ni = 1;
        }

        // Evaluate the design
        rot_top->eval();
        m_trace->dump(i);

        // Apply some basic input stimuli
        // ...

        // Check output signals
        // ...

        // Advance the simulation time
        Verilated::timeInc(10);
    }

    // Finalize the simulation
    rot_top->final();
    m_trace->close();

    // Clean up
    delete rot_top;

    cout << "Sim Finished!" << endl;

    return 0;
}