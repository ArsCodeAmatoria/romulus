'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Button } from '@/components/ui/button';
import CodeEditor from './CodeEditor';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import dynamic from 'next/dynamic';
import { LoadingSpinner } from '@/components/interactive/LoadingSpinner';

// Dynamically import Plotly to avoid SSR issues
const Plot = dynamic(() => import('react-plotly.js'), { ssr: false });

// Use dynamic import for PlotlyJS as well
const PlotlyJS = dynamic(() => import('plotly.js'), { 
  ssr: false,
  loading: () => null 
});

// Default Python code for the Emergent Gravity model
const DEFAULT_CODE = `import numpy as np
import json

# Print some info for testing console output
print("Starting gravity calculations...")

# Constants
G = 6.67e-11  # Gravitational constant

def emergent_gravity_acceleration(r, M, a0=1.2e-10):
    """
    Calculate acceleration due to emergent gravity effects
    
    Parameters:
    -----------
    r : float or array
        Radius from center (meters)
    M : float
        Baryonic mass (kg)
    a0 : float
        Acceleration scale (m/s²)
        
    Returns:
    --------
    float or array : Total acceleration
    """
    # Newtonian acceleration
    a_N = G * M / r**2
    
    # Emergent gravity contribution
    a_DM = np.sqrt(a0 * a_N)
    
    # Total observed acceleration
    a_total = a_N + a_DM
    
    return a_total

# Galaxy parameters
M_galaxy = 1e10 * 2e30  # Galaxy mass (10 billion solar masses in kg)
print(f"Galaxy mass: {M_galaxy/2e30:.2e} solar masses")

# Radii from 1 to 30 kpc
r_values = np.linspace(1, 30, 100) * 3.086e19  # kpc to meters

# Calculate accelerations
a_newton = G * M_galaxy / r_values**2
a_total = emergent_gravity_acceleration(r_values, M_galaxy)

# Convert to velocities
v_newton = np.sqrt(r_values * a_newton)
v_total = np.sqrt(r_values * a_total)

# Report some metrics for comparison
print(f"Velocity at 10 kpc (Newtonian): {v_newton[33]:.2f} m/s")
print(f"Velocity at 10 kpc (Emergent): {v_total[33]:.2f} m/s")
print(f"Ratio of velocities: {v_total[33]/v_newton[33]:.2f}x")

# Convert to kpc for plotting
r_kpc = r_values / 3.086e19

# Create plot data
trace1 = {
    'x': r_kpc.tolist(),
    'y': v_newton.tolist(),
    'mode': 'lines',
    'name': 'Newtonian Gravity',
    'line': {'color': 'rgba(255, 255, 255, 0.7)'}
}

trace2 = {
    'x': r_kpc.tolist(),
    'y': v_total.tolist(),
    'mode': 'lines',
    'name': 'Emergent Gravity',
    'line': {'color': 'rgba(255, 20, 147, 1)'}
}

# Plot layout
layout = {
    'title': 'Galaxy Rotation Curve',
    'xaxis': {
        'title': 'Radius (kpc)',
        'color': 'white'
    },
    'yaxis': {
        'title': 'Velocity (m/s)',
        'color': 'white'
    },
    'plot_bgcolor': 'rgba(0, 0, 0, 0)',
    'paper_bgcolor': 'rgba(0, 0, 0, 0)',
    'font': {
        'color': 'white'
    },
    'legend': {
        'font': {
            'color': 'white'
        }
    }
}

print("Calculations complete!")

# Create the figure and store for visualization
figure = {'data': [trace1, trace2], 'layout': layout}
plotly_data = figure  # Store as Python object, will be auto-converted

print("Plot data generated successfully")`;

interface PyodideTerminalProps {
  initialCode?: string;
}

interface PyodideType {
  runPythonAsync: (code: string) => Promise<any>;
  globals: {
    get: (key: string) => any;
  };
  registerJsModule: (name: string, module: any) => void;
  setStdout: (options: { write: (text: string) => void }) => void;
  setStderr: (options: { write: (text: string) => void }) => void;
}

export function PyodideTerminal({
  initialCode = DEFAULT_CODE,
}: PyodideTerminalProps) {
  const [code, setCode] = useState(initialCode);
  const [output, setOutput] = useState<string>('');
  const [isLoading, setIsLoading] = useState(false);
  const [pyodideLoading, setPyodideLoading] = useState(false);
  const [pyodideReady, setPyodideReady] = useState(false);
  const [plotData, setPlotData] = useState<any>(null);
  const [activeTab, setActiveTab] = useState<string>('code');
  const [plotlyModule, setPlotlyModule] = useState<any>(null);

  const pyodideRef = useRef<PyodideType | null>(null);

  // Initialize Pyodide
  useEffect(() => {
    async function initPyodide() {
      try {
        setPyodideLoading(true);
        console.log('Loading pyodide...');
        
        // Load Pyodide without stdout/stderr initially
        // @ts-ignore
        const pyodide = await window.loadPyodide({
          indexURL: "https://cdn.jsdelivr.net/pyodide/v0.24.1/full/"
        });
        
        pyodideRef.current = pyodide;
        
        // Load numpy first
        await pyodide.loadPackagesFromImports('import numpy as np');
        console.log('NumPy loaded successfully');
        
        // Only register Plotly module after it's loaded on the client side
        if (typeof PlotlyJS !== 'function') {
          // If PlotlyJS is a dynamic import (function), wait for it to resolve
          const plotlyImport = await PlotlyJS;
          setPlotlyModule(plotlyImport);
          if (plotlyImport) {
            pyodide.registerJsModule('plotly', plotlyImport);
            console.log('Plotly registered with Pyodide');
          }
        }
        
        setPyodideReady(true);
        console.log('Pyodide loaded successfully');
      } catch (error) {
        console.error('Error initializing Pyodide:', error);
        setOutput(`Error initializing Python environment: ${error instanceof Error ? error.message : String(error)}`);
      } finally {
        setPyodideLoading(false);
      }
    }

    // Only run in browser
    if (typeof window !== 'undefined') {
      // Load Pyodide script
      const script = document.createElement('script');
      script.src = 'https://cdn.jsdelivr.net/pyodide/v0.24.1/full/pyodide.js';
      script.async = true;
      script.onload = () => {
        initPyodide();
      };
      document.body.appendChild(script);
  
      return () => {
        if (document.body.contains(script)) {
          document.body.removeChild(script);
        }
      };
    }
  }, []);

  // Register Plotly module when plotlyModule changes
  useEffect(() => {
    if (pyodideRef.current && plotlyModule) {
      pyodideRef.current.registerJsModule('plotly', plotlyModule);
      console.log('Plotly registered with Pyodide after dynamic import');
    }
  }, [plotlyModule]);

  const runCode = async () => {
    if (!pyodideRef.current) {
      setOutput('Python environment not ready yet. Please wait...');
      return;
    }

    setIsLoading(true);
    setOutput('');
    setActiveTab('output');
    
    try {
      // Execute the code with a much simpler approach
      const simpleCode = `
import sys
import io
import json
import traceback

# Create a function to execute the user code safely
def run_user_code():
    # Store original stdout
    original_stdout = sys.stdout
    
    # Create a string buffer for output
    stdout_buffer = io.StringIO()
    
    # Redirect stdout to our buffer
    sys.stdout = stdout_buffer
    
    try:
        # Execute the user code directly
${code.split('\n').map(line => '        ' + line).join('\n')}
        
        # Get information about plot data
        has_plot = 'plotly_data' in globals()
        plot_data = globals().get('plotly_data', None) if has_plot else None
        return {
            "success": True,
            "output": stdout_buffer.getvalue(),
            "has_plot": has_plot,
            "plot_data": plot_data,
            "error": None
        }
    except Exception as e:
        return {
            "success": False,
            "output": stdout_buffer.getvalue(),
            "has_plot": False,
            "plot_data": None,
            "error": str(e),
            "traceback": traceback.format_exc()
        }
    finally:
        # Reset stdout
        sys.stdout = original_stdout
        # Close buffer
        stdout_buffer.close()

# Run the code and return the result
result = run_user_code()
result
`;

      // Run the code
      const result = await pyodideRef.current.runPythonAsync(simpleCode);
      
      console.log("Python execution result:", result);
      
      // Process the result
      let outputText = '';
      
      if (result && typeof result === 'object') {
        // Handle the output
        if (result.output && result.output.length > 0) {
          outputText = result.output;
        }
        
        // Handle any error
        if (!result.success && result.error) {
          outputText += `\n\nError: ${result.error}\n`;
          if (result.traceback) {
            outputText += `\n${result.traceback}`;
          }
        }
        
        // Handle plot data
        if (result.has_plot && result.plot_data) {
          try {
            setPlotData(result.plot_data);
            // If we have output, show that first, then switch to visualization after a delay
            if (outputText) {
              setTimeout(() => setActiveTab('visualization'), 1500);
            } else {
              setActiveTab('visualization');
            }
          } catch (e: any) {
            console.error('Failed to handle plot data:', e);
            outputText += `\n\nVisualization Error: ${e.message}`;
          }
        }
      }
      
      if (outputText.trim() === '') {
        setOutput('No output was produced. Try adding some print statements to your code.');
      } else {
        setOutput(outputText);
      }
    } catch (error) {
      console.error('Error running Python code:', error);
      setOutput(`Error: ${error instanceof Error ? error.message : String(error)}`);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="flex flex-col space-y-4">
      <Card className="bg-black border-dark-pink/20">
        <CardHeader>
          <CardTitle className="text-white flex items-center justify-between">
            <span>Interactive Python Terminal</span>
            {pyodideLoading ? (
              <span className="text-sm text-dark-pink/80 flex items-center">
                <LoadingSpinner /> Loading Python Environment...
              </span>
            ) : pyodideReady ? (
              <span className="text-sm text-green-500">Ready</span>
            ) : (
              <span className="text-sm text-red-500">Error Loading Python</span>
            )}
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="mb-4">
            <CodeEditor
              initialCode={code}
              onChange={setCode}
              height="350px"
            />
          </div>
          <div className="flex justify-between">
            <Button
              className="bg-dark-pink hover:bg-dark-pink/80 text-white"
              onClick={runCode}
              disabled={isLoading || !pyodideReady}
            >
              {isLoading ? <><LoadingSpinner /> Running...</> : 'Run Code'}
            </Button>
          </div>

          <div className="mt-6">
            <Tabs value={activeTab} onValueChange={setActiveTab}>
              <TabsList className="bg-zinc-900/50">
                <TabsTrigger value="code" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">
                  Code View
                </TabsTrigger>
                <TabsTrigger value="output" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">
                  Console Output
                </TabsTrigger>
                <TabsTrigger value="visualization" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">
                  Visualization
                </TabsTrigger>
              </TabsList>

              <TabsContent value="code" className="mt-4">
                <div className="bg-zinc-900/50 p-4 rounded-md">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Code Explanation</h3>
                  <p className="text-white/80">
                    This code calculates how galaxies rotate under standard Newtonian gravity (white) 
                    and under Erik Verlinde's Emergent Gravity theory (pink). Modify the mass, 
                    radii, or <code>a0</code> parameter to see how they affect the predicted rotation curves.
                  </p>
                </div>
              </TabsContent>

              <TabsContent value="output" className="mt-4">
                <div className="bg-zinc-900/50 p-4 rounded-md h-48 overflow-auto font-mono text-sm text-white/80 whitespace-pre-wrap">
                  {output || 'Run the code to see output here.'}
                </div>
              </TabsContent>

              <TabsContent value="visualization" className="mt-4">
                <div className="bg-zinc-900/50 p-4 rounded-md flex justify-center">
                  {plotData ? (
                    <div className="w-full h-96">
                      <Plot
                        data={plotData.data}
                        layout={{
                          ...plotData.layout,
                          autosize: true,
                          height: 380,
                        }}
                        useResizeHandler={true}
                        style={{ width: '100%', height: '100%' }}
                      />
                    </div>
                  ) : (
                    <div className="text-white/80 h-96 flex items-center justify-center">
                      Run the code to generate a visualization.
                    </div>
                  )}
                </div>
              </TabsContent>
            </Tabs>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

export default PyodideTerminal; 