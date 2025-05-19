'use client';

import { useEffect, useState } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { Card } from "@/components/ui/card";
import { SimulationData, loadSimulationData } from '@/lib/agdef/data';

export function SimulationPlots() {
  const [data, setData] = useState<SimulationData[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function fetchData() {
      const simulationData = await loadSimulationData();
      setData(simulationData);
      setLoading(false);
    }
    fetchData();
  }, []);

  if (loading) {
    return (
      <div className="flex items-center justify-center h-[400px]">
        <p className="text-gray-600">Loading simulation data...</p>
      </div>
    );
  }

  if (data.length === 0) {
    return (
      <div className="flex items-center justify-center h-[400px]">
        <p className="text-gray-600">No simulation data available</p>
      </div>
    );
  }

  return (
    <div className="space-y-8">
      <Card className="p-6">
        <h3 className="text-xl font-medium mb-4">Distance Modulus vs Redshift</h3>
        <div className="h-[400px]">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="z" 
                label={{ value: 'Redshift (z)', position: 'insideBottom', offset: -5 }} 
              />
              <YAxis 
                label={{ value: 'Distance Modulus (μ)', angle: -90, position: 'insideLeft' }} 
              />
              <Tooltip 
                contentStyle={{
                  background: "#18181b",
                  color: "#fff",
                  border: "1px solid #a21caf",
                  borderRadius: "8px"
                }}
                itemStyle={{
                  color: "#fff"
                }}
              />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="mu_agdef" 
                name="AGDEF" 
                stroke="#8884d8" 
                strokeWidth={2}
              />
              <Line 
                type="monotone" 
                dataKey="mu_lcdm" 
                name="ΛCDM" 
                stroke="#82ca9d" 
                strokeWidth={2}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </Card>

      <Card className="p-6">
        <h3 className="text-xl font-medium mb-4">Luminosity Distance vs Redshift</h3>
        <div className="h-[400px]">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="z" 
                label={{ value: 'Redshift (z)', position: 'insideBottom', offset: -5 }} 
              />
              <YAxis 
                label={{ value: 'Luminosity Distance (Mpc)', angle: -90, position: 'insideLeft' }} 
              />
              <Tooltip 
                contentStyle={{
                  background: "#18181b",
                  color: "#fff",
                  border: "1px solid #a21caf",
                  borderRadius: "8px"
                }}
                itemStyle={{
                  color: "#fff"
                }}
              />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="dL_agdef" 
                name="AGDEF" 
                stroke="#8884d8" 
                strokeWidth={2}
              />
              <Line 
                type="monotone" 
                dataKey="dL_lcdm" 
                name="ΛCDM" 
                stroke="#82ca9d" 
                strokeWidth={2}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </Card>
    </div>
  );
} 