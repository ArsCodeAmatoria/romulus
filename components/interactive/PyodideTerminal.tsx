'use client';

import React, { useState, useEffect, useRef } from 'react';
import { Button } from '@/components/ui/button';
import CodeEditor from './CodeEditor';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import dynamic from 'next/dynamic';
import { LoadingSpinner } from './LoadingSpinner';
import * as PlotlyJS from 'plotly.js';

// Dynamically import Plotly to avoid SSR issues
const Plot = dynamic(() => import('react-plotly.js'), { ssr: false });

// Default Python code for the Emergent Gravity model
const DEFAULT_CODE = `import numpy as np
import json

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

# Radii from 1 to 30 kpc
r_values = np.linspace(1, 30, 100) * 3.086e19  # kpc to meters

# Calculate accelerations
a_newton = G * M_galaxy / r_values**2
a_total = emergent_gravity_acceleration(r_values, M_galaxy)

# Convert to velocities
v_newton = np.sqrt(r_values * a_newton)
v_total = np.sqrt(r_values * a_total)

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

# Create the figure
figure = {'data': [trace1, trace2], 'layout': layout}

# Convert to JSON for Plotly
plotly_data = json.dumps(figure)

# Return the plot data to be rendered by React
plotly_data`;

interface PyodideTerminalProps {
  initialCode?: string;
}

interface PyodideType {
  runPythonAsync: (code: string) => Promise<any>;
  globals: {
    get: (key: string) => any;
  };
  registerJsModule: (name: string, module: any) => void;
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

  const pyodideRef = useRef<PyodideType | null>(null);

  // Initialize Pyodide
  useEffect(() => {
    async function initPyodide() {
      try {
        setPyodideLoading(true);
        
        // For development mode, we'll log to console
        console.log('Loading pyodide...');
        
        // @ts-ignore
        const pyodide = await window.loadPyodide({
          indexURL: "https://cdn.jsdelivr.net/pyodide/v0.24.1/full/",
        });
        
        pyodideRef.current = pyodide;
        
        // Register Plotly as a JavaScript module that can be imported from Python
        pyodide.registerJsModule('plotly', PlotlyJS);
        
        // Pre-install numpy
        await pyodide.loadPackagesFromImports('import numpy as np');
        
        setPyodideReady(true);
        console.log('Pyodide loaded successfully');
      } catch (error) {
        console.error('Error initializing Pyodide:', error);
        setOutput('Error initializing Python environment. Please try again later.');
      } finally {
        setPyodideLoading(false);
      }
    }

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
  }, []);

  const runCode = async () => {
    if (!pyodideRef.current) {
      setOutput('Python environment not ready yet. Please wait...');
      return;
    }

    setIsLoading(true);
    setOutput('');
    setActiveTab('output');

    try {
      // Capture stdout and stderr
      const stdout: string[] = [];
      const stderr: string[] = [];
      
      // @ts-ignore
      pyodideRef.current.setStdout({ write: (text) => stdout.push(text) });
      // @ts-ignore
      pyodideRef.current.setStderr({ write: (text) => stderr.push(text) });

      // Run the code
      const result = await pyodideRef.current.runPythonAsync(code);

      // Format output
      let outputText = '';
      if (stdout.length > 0) {
        outputText += stdout.join('');
      }
      if (stderr.length > 0) {
        outputText += '\nErrors:\n' + stderr.join('');
      }
      
      setOutput(outputText);

      // Check if the result is a plot data JSON
      if (result && typeof result === 'string' && result.includes('"data"') && result.includes('"layout"')) {
        try {
          const plotJson = JSON.parse(result);
          setPlotData(plotJson);
          setActiveTab('visualization');
        } catch (e) {
          console.error('Failed to parse plot data:', e);
        }
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
                <div className="bg-zinc-900/50 p-4 rounded-md h-48 overflow-auto font-mono text-sm text-white/80">
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