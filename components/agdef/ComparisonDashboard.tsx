'use client';

import { useState, useEffect, useMemo } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Slider } from "@/components/ui/slider";
import { Switch } from "@/components/ui/switch";
import { Label } from "@/components/ui/label";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Line } from 'react-chartjs-2';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
} from 'chart.js';

// Register ChartJS components
ChartJS.register(
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend
);

interface AGDEFParams {
  kappa: number;        // Anti-gravity coupling constant
  omegaDE: number;      // Dark energy density parameter
  evolutionRate: number; // Rate of dark energy evolution
}

export function ComparisonDashboard() {
  // State for model parameters
  const [params, setParams] = useState<AGDEFParams>({
    kappa: 1.0,
    omegaDE: 0.7,
    evolutionRate: 0.05
  });

  // State for model selection
  const [showLCDM, setShowLCDM] = useState(true);
  const [showAGDEF, setShowAGDEF] = useState(true);

  // State for data
  const [pantheonData, setPantheonData] = useState<any>(null);
  const [planckData, setPlanckData] = useState<{ l: number; cl: number; clErr: number }[] | null>(null);

  // Add loading state
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Update data loading with error handling
  useEffect(() => {
    setIsLoading(true);
    setError(null);
    
    Promise.all([
      fetch('/data/pantheon_plus.csv')
        .then(response => {
          if (!response.ok) throw new Error('Failed to load Pantheon+ data');
          return response.text();
        })
        .then(csv => {
          const rows = csv.split('\n').slice(1);
          const data = rows.map(row => {
            const [z, mu, muErr] = row.split(',').map(Number);
            return { z, mu, muErr };
          });
          setPantheonData(data);
        }),
      fetch('/data/planck_cmb_isw.csv')
        .then(response => {
          if (!response.ok) throw new Error('Failed to load Planck CMB data');
          return response.text();
        })
        .then(csv => {
          const rows = csv.split('\n').slice(1);
          const data = rows.map(row => {
            const [l, cl, clErr] = row.split(',').map(Number);
            return { l, cl, clErr };
          });
          setPlanckData(data);
        })
    ]).catch(err => {
      setError(err.message);
      console.error('Error loading data:', err);
    }).finally(() => {
      setIsLoading(false);
    });
  }, []);

  // Compute AGDEF predictions
  const computeAGDEFPredictions = () => {
    const zVals = Array.from({ length: 100 }, (_, i) => i * 0.02);
    return zVals.map(z => {
      const rhoAGDEF = params.kappa * (params.omegaDE + params.evolutionRate * Math.exp(-z));
      const hsq = 0.3 * Math.pow(1 + z, 3) + rhoAGDEF;
      const dl = (1 + z) * 2997.92458 * (1 / Math.sqrt(hsq)); // Simplified
      const mu = 5 * Math.log10(dl * 1e6 / 10);
      return { z, mu };
    });
  };

  // Compute ΛCDM predictions
  const computeLCDMPredictions = () => {
    const zVals = Array.from({ length: 100 }, (_, i) => i * 0.02);
    return zVals.map(z => {
      const hsq = 0.3 * Math.pow(1 + z, 3) + 0.7;
      const dl = (1 + z) * 2997.92458 * (1 / Math.sqrt(hsq)); // Simplified
      const mu = 5 * Math.log10(dl * 1e6 / 10);
      return { z, mu };
    });
  };

  // Compute AGDEF CMB predictions with ISW effect
  const computeAGDEFCMB = () => {
    if (!planckData) return [];
    return planckData.map((d: { l: number }) => {
      const l = d.l;
      // Base CMB power spectrum (simplified)
      const baseCl = 1 / (l * (l + 1));
      
      // ISW effect contribution (enhanced at low l)
      const iswEffect = params.kappa * (params.omegaDE + params.evolutionRate * Math.exp(-l / 30));
      
      // Sachs-Wolfe effect (dominant at low l)
      const swEffect = 1.0;
      
      // Total power spectrum
      const totalCl = baseCl * (swEffect + iswEffect);
      
      return { x: l, y: totalCl };
    });
  };

  // Compute LCDM CMB predictions
  const computeLCDMCMB = () => {
    if (!planckData) return [];
    return planckData.map((d: { l: number }) => {
      const l = d.l;
      // Base CMB power spectrum (simplified)
      const baseCl = 1 / (l * (l + 1));
      
      // LCDM ISW effect (constant)
      const iswEffect = 0.7;
      
      // Sachs-Wolfe effect
      const swEffect = 1.0;
      
      // Total power spectrum
      const totalCl = baseCl * (swEffect + iswEffect);
      
      return { x: l, y: totalCl };
    });
  };

  // Prepare chart data
  const chartData = {
    datasets: [
      {
        label: 'Pantheon+ Data',
        data: pantheonData?.map((d: any) => ({ x: d.z, y: d.mu })),
        backgroundColor: 'rgba(255, 99, 132, 0.5)',
        borderColor: 'rgb(255, 99, 132)',
        pointRadius: 2,
        showLine: false,
      },
      ...(showLCDM ? [{
        label: 'ΛCDM',
        data: computeLCDMPredictions().map(d => ({ x: d.z, y: d.mu })),
        borderColor: 'rgb(54, 162, 235)',
        backgroundColor: 'rgba(54, 162, 235, 0.5)',
        borderWidth: 2,
        pointRadius: 0,
      }] : []),
      ...(showAGDEF ? [{
        label: 'AGDEF',
        data: computeAGDEFPredictions().map(d => ({ x: d.z, y: d.mu })),
        borderColor: 'rgb(75, 192, 192)',
        backgroundColor: 'rgba(75, 192, 192, 0.5)',
        borderWidth: 2,
        pointRadius: 0,
      }] : []),
    ],
  };

  // Chart options
  const chartOptions = {
    responsive: true,
    plugins: {
      legend: {
        position: 'top' as const,
      },
      title: {
        display: true,
        text: 'Distance Modulus vs Redshift',
      },
    },
    scales: {
      x: {
        type: 'linear' as const,
        title: {
          display: true,
          text: 'Redshift (z)',
        },
      },
      y: {
        title: {
          display: true,
          text: 'Distance Modulus (μ)',
        },
      },
    },
  };

  // Prepare CMB chart data (useMemo to ensure reactivity)
  const cmbChartData = useMemo(() => ({
    datasets: [
      {
        label: 'Planck Data',
        data: planckData?.map((d: any) => ({ x: d.l, y: d.cl })),
        backgroundColor: 'rgba(255, 99, 132, 0.5)',
        borderColor: 'rgb(255, 99, 132)',
        pointRadius: 2,
        showLine: false,
      },
      ...(showLCDM ? [{
        label: 'ΛCDM',
        data: computeLCDMCMB(),
        borderColor: 'rgb(54, 162, 235)',
        backgroundColor: 'rgba(54, 162, 235, 0.5)',
        borderWidth: 2,
        pointRadius: 0,
        showLine: true,
      }] : []),
      ...(showAGDEF ? [{
        label: 'AGDEF',
        data: computeAGDEFCMB(),
        borderColor: 'rgb(75, 192, 192)',
        backgroundColor: 'rgba(75, 192, 192, 0.5)',
        borderWidth: 2,
        pointRadius: 0,
        showLine: true,
      }] : []),
    ],
  }), [planckData, showLCDM, showAGDEF, params]);

  // CMB chart options
  const cmbChartOptions = {
    responsive: true,
    plugins: {
      legend: {
        position: 'top' as const,
      },
      title: {
        display: true,
        text: 'CMB Power Spectrum vs Multipole Moment',
      },
      tooltip: {
        backgroundColor: '#18181b',
        titleColor: '#fff',
        bodyColor: '#fff',
        borderColor: '#a21caf',
        borderWidth: 1,
      },
    },
    scales: {
      x: {
        type: 'linear' as const,
        title: {
          display: true,
          text: 'Multipole Moment (ℓ)',
        },
      },
      y: {
        title: {
          display: true,
          text: 'Power Spectrum (Cℓ)',
        },
      },
    },
  };

  // Add tooltips for parameters
  const parameterTooltips = {
    kappa: "Anti-gravity coupling constant (κ) that determines the strength of the repulsive force",
    omegaDE: "Dark energy density parameter (ΩDE) representing the current energy density of dark energy",
    evolutionRate: "Rate at which the dark energy density evolves with redshift"
  };

  // Update the UI to include loading states and error handling
  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-[400px]">
        <div className="text-white">Loading data...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="flex items-center justify-center h-[400px]">
        <div className="text-red-500">Error: {error}</div>
      </div>
    );
  }

  // Update the parameter sliders section with tooltips
  const renderParameterSlider = (
    label: string,
    value: number,
    onChange: (value: number) => void,
    min: number,
    max: number,
    step: number,
    tooltip: string
  ) => (
    <div className="space-y-2">
      <div className="flex items-center justify-between">
        <Label className="text-white">{label}</Label>
        <div className="text-sm text-white/60">{value.toFixed(2)}</div>
      </div>
      <Slider
        value={[value]}
        onValueChange={([v]) => onChange(v)}
        min={min}
        max={max}
        step={step}
        className="mt-2"
      />
      <p className="text-sm text-white/60">{tooltip}</p>
    </div>
  );

  return (
    <div className="space-y-6">
      <Card className="bg-black border-dark-pink/20">
        <CardHeader>
          <CardTitle className="text-white">AGDEF vs ΛCDM Comparison</CardTitle>
        </CardHeader>
        <CardContent>
          <Tabs defaultValue="supernovae" className="space-y-4">
            <TabsList className="bg-black border border-dark-pink/20">
              <TabsTrigger value="supernovae" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">
                Supernovae
              </TabsTrigger>
              <TabsTrigger value="cmb" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">
                CMB
              </TabsTrigger>
            </TabsList>

            <TabsContent value="supernovae" className="space-y-4">
              {/* Model Toggles */}
              <div className="flex space-x-4">
                <div className="flex items-center space-x-2">
                  <Switch
                    id="lcdm"
                    checked={showLCDM}
                    onCheckedChange={setShowLCDM}
                  />
                  <Label htmlFor="lcdm" className="text-white">Show ΛCDM</Label>
                </div>
                <div className="flex items-center space-x-2">
                  <Switch
                    id="agdef"
                    checked={showAGDEF}
                    onCheckedChange={setShowAGDEF}
                  />
                  <Label htmlFor="agdef" className="text-white">Show AGDEF</Label>
                </div>
              </div>

              {/* AGDEF Parameters with tooltips */}
              <div className="space-y-4">
                {renderParameterSlider(
                  "Anti-gravity Coupling (κ)",
                  params.kappa,
                  (value) => setParams({ ...params, kappa: value }),
                  0.5,
                  2.0,
                  0.1,
                  parameterTooltips.kappa
                )}
                {renderParameterSlider(
                  "Dark Energy Density (ΩDE)",
                  params.omegaDE,
                  (value) => setParams({ ...params, omegaDE: value }),
                  0.6,
                  0.8,
                  0.01,
                  parameterTooltips.omegaDE
                )}
                {renderParameterSlider(
                  "Evolution Rate",
                  params.evolutionRate,
                  (value) => setParams({ ...params, evolutionRate: value }),
                  0.0,
                  0.1,
                  0.01,
                  parameterTooltips.evolutionRate
                )}
              </div>

              {/* Supernovae Plot */}
              <div className="h-[400px] bg-zinc-900/50 p-4 rounded-md">
                <Line data={chartData} options={chartOptions} />
              </div>
            </TabsContent>

            <TabsContent value="cmb" className="space-y-4">
              {/* CMB Plot: AGDEF vs LCDM vs Planck */}
              <div className="h-[400px] bg-zinc-900/50 p-4 rounded-md">
                <Line data={cmbChartData} options={cmbChartOptions} />
              </div>
            </TabsContent>
          </Tabs>
        </CardContent>
      </Card>
    </div>
  );
} 