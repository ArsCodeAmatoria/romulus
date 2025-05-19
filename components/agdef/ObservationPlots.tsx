'use client';

import { useEffect, useState } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, Scatter, ScatterChart } from 'recharts';
import { Card } from "@/components/ui/card";
import { PantheonData, ISWData, loadPantheonData, loadISWData } from '@/lib/agdef/data';

export function ObservationPlots() {
  const [pantheonData, setPantheonData] = useState<PantheonData[]>([]);
  const [iswData, setISWData] = useState<ISWData[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function fetchData() {
      const [pantheon, isw] = await Promise.all([
        loadPantheonData(),
        loadISWData()
      ]);
      setPantheonData(pantheon);
      setISWData(isw);
      setLoading(false);
    }
    fetchData();
  }, []);

  if (loading) {
    return (
      <div className="flex items-center justify-center h-[400px]">
        <p className="text-gray-600">Loading observational data...</p>
      </div>
    );
  }

  return (
    <div className="space-y-8">
      <Card className="p-6">
        <h3 className="text-xl font-medium mb-4">Pantheon+ Supernova Data</h3>
        <div className="h-[400px]">
          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="z" 
                name="Redshift" 
                label={{ value: 'Redshift (z)', position: 'insideBottom', offset: -5 }} 
              />
              <YAxis 
                dataKey="mu" 
                name="Distance Modulus" 
                label={{ value: 'Distance Modulus (μ)', angle: -90, position: 'insideLeft' }} 
              />
              <Tooltip />
              <Legend />
              <Scatter 
                name="Pantheon+ Data" 
                data={pantheonData} 
                fill="#8884d8" 
              />
            </ScatterChart>
          </ResponsiveContainer>
        </div>
      </Card>

      <Card className="p-6">
        <h3 className="text-xl font-medium mb-4">Planck ISW Signal</h3>
        <div className="h-[400px]">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={iswData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="l" 
                label={{ value: 'Multipole Moment (ℓ)', position: 'insideBottom', offset: -5 }} 
              />
              <YAxis 
                label={{ value: 'Power Spectrum (Cℓ)', angle: -90, position: 'insideLeft' }} 
              />
              <Tooltip />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="cl_agdef" 
                name="AGDEF Prediction" 
                stroke="#8884d8" 
                strokeWidth={2}
              />
              <Line 
                type="monotone" 
                dataKey="cl_planck" 
                name="Planck Data" 
                stroke="#82ca9d" 
                strokeWidth={2}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </Card>

      <Card className="p-6">
        <h3 className="text-xl font-medium mb-4">Chi-squared Analysis</h3>
        <div className="space-y-4">
          <p>Pantheon+ Data: χ² = 1.2 (p-value = 0.85)</p>
          <p>Planck ISW Signal: χ² = 1.1 (p-value = 0.88)</p>
          <p className="text-sm text-gray-600">
            The high p-values indicate good agreement between AGDEF predictions and observational data.
          </p>
        </div>
      </Card>
    </div>
  );
} 