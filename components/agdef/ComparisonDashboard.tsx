'use client';

import { useState, useEffect } from 'react';
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

  // Load Pantheon+ data
  useEffect(() => {
    fetch('/data/pantheon_plus.csv')
      .then(response => response.text())
      .then(csv => {
        // Parse CSV and set data
        const rows = csv.split('\n').slice(1); // Skip header
        const data = rows.map(row => {
          const [z, mu, muErr] = row.split(',').map(Number);
          return { z, mu, muErr };
        });
        setPantheonData(data);
      });
  }, []);

  // Load Planck data
  useEffect(() => {
    fetch('/data/planck_cmb_isw.csv')
      .then(response => response.text())
      .then(csv => {
        // Parse CSV and set data
        const rows = csv.split('\n').slice(1); // Skip header
        const data = rows.map(row => {
          const [l, cl, clErr] = row.split(',').map(Number);
          return { l, cl, clErr };
        });
        setPlanckData(data);
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

  // Compute AGDEF CMB predictions (simple demo model)
  const computeAGDEFCMB = () => {
    if (!planckData) return [];
    return planckData.map((d: { l: number }) => {
      const l = d.l;
      // AGDEF modifies ISW at low l
      const base = 1; // Placeholder for base power spectrum
      const agdefEffect = params.kappa * (params.omegaDE + params.evolutionRate * Math.exp(-l / 30));
      return { x: l, y: base * (1 + agdefEffect) };
    });
  };

  // Compute LCDM CMB predictions (simple demo model)
  const computeLCDMCMB = () => {
    if (!planckData) return [];
    return planckData.map((d: { l: number }) => {
      const l = d.l;
      const base = 1; // Placeholder for base power spectrum
      const lcdmEffect = 0.7; // Fixed for LCDM
      return { x: l, y: base * (1 + lcdmEffect) };
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

  // Prepare CMB chart data
  const cmbChartData = {
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
  };

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

              {/* AGDEF Parameters */}
              <div className="space-y-4">
                <div>
                  <Label className="text-white">Anti-gravity Coupling (κ)</Label>
                  <Slider
                    value={[params.kappa]}
                    onValueChange={([value]) => setParams({ ...params, kappa: value })}
                    min={0.5}
                    max={2.0}
                    step={0.1}
                    className="mt-2"
                  />
                </div>
                <div>
                  <Label className="text-white">Dark Energy Density (ΩDE)</Label>
                  <Slider
                    value={[params.omegaDE]}
                    onValueChange={([value]) => setParams({ ...params, omegaDE: value })}
                    min={0.6}
                    max={0.8}
                    step={0.01}
                    className="mt-2"
                  />
                </div>
                <div>
                  <Label className="text-white">Evolution Rate</Label>
                  <Slider
                    value={[params.evolutionRate]}
                    onValueChange={([value]) => setParams({ ...params, evolutionRate: value })}
                    min={0.0}
                    max={0.1}
                    step={0.01}
                    className="mt-2"
                  />
                </div>
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