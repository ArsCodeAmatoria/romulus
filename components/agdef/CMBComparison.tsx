"use client"

import { useState, useEffect } from "react"
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { Switch } from "@/components/ui/switch"
import { Label } from "@/components/ui/label"
import { Slider } from "@/components/ui/slider"
import { Line } from "react-chartjs-2"
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  LogarithmicScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
} from "chart.js"

ChartJS.register(
  CategoryScale,
  LinearScale,
  LogarithmicScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend
)

interface CMBData {
  ell: number[]
  cl: number[]
  error: number[]
}

export function CMBComparison() {
  const [showPlanck, setShowPlanck] = useState(true)
  const [showAGDEF, setShowAGDEF] = useState(true)
  const [showISW, setShowISW] = useState(true)
  const [agdefStrength, setAgdefStrength] = useState(0.7)
  const [cmbData, setCmbData] = useState<CMBData | null>(null)

  useEffect(() => {
    // Load Planck CMB data
    fetch("/data/planck_cmb.json")
      .then((res) => res.json())
      .then((data) => setCmbData(data))
      .catch((err) => console.error("Error loading CMB data:", err))
  }, [])

  const generateAGDEFPrediction = () => {
    if (!cmbData) return null

    // Generate AGDEF prediction based on current strength
    return cmbData.ell.map((ell) => {
      // AGDEF modifies the power spectrum at low ell (large scales)
      const baseCl = cmbData.cl[cmbData.ell.indexOf(ell)]
      const iswEnhancement = ell < 30 ? agdefStrength * 0.2 : 0
      return baseCl * (1 + iswEnhancement)
    })
  }

  const generateISWEffect = () => {
    if (!cmbData) return null

    // Generate ISW effect prediction
    return cmbData.ell.map((ell) => {
      // ISW effect is most prominent at low ell
      const baseCl = cmbData.cl[cmbData.ell.indexOf(ell)]
      const iswContribution = ell < 30 ? agdefStrength * 0.15 * baseCl : 0
      return iswContribution
    })
  }

  const chartData = {
    labels: cmbData?.ell || [],
    datasets: [
      {
        label: "Planck 2018",
        data: cmbData?.cl || [],
        borderColor: "rgb(75, 192, 192)",
        backgroundColor: "rgba(75, 192, 192, 0.5)",
        hidden: !showPlanck,
      },
      {
        label: "AGDEF Prediction",
        data: generateAGDEFPrediction() || [],
        borderColor: "rgb(255, 99, 132)",
        backgroundColor: "rgba(255, 99, 132, 0.5)",
        hidden: !showAGDEF,
      },
      {
        label: "ISW Effect",
        data: generateISWEffect() || [],
        borderColor: "rgb(153, 102, 255)",
        backgroundColor: "rgba(153, 102, 255, 0.5)",
        hidden: !showISW,
      },
    ],
  }

  const chartOptions = {
    responsive: true,
    plugins: {
      legend: {
        position: "top" as const,
      },
      title: {
        display: true,
        text: "CMB Power Spectrum Comparison",
      },
      tooltip: {
        backgroundColor: "#18181b",
        titleColor: "#fff",
        bodyColor: "#fff",
        borderColor: "#a21caf",
        borderWidth: 1,
      },
    },
    scales: {
      x: {
        type: "logarithmic" as const,
        title: {
          display: true,
          text: "Multipole Moment (ℓ)",
        },
      },
      y: {
        title: {
          display: true,
          text: "ℓ(ℓ+1)Cℓ/2π [μK²]",
        },
      },
    },
  }

  return (
    <Card className="w-full">
      <CardHeader>
        <CardTitle>CMB Power Spectrum Analysis</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          <div className="flex items-center space-x-4">
            <div className="flex items-center space-x-2">
              <Switch
                id="show-planck"
                checked={showPlanck}
                onCheckedChange={setShowPlanck}
              />
              <Label htmlFor="show-planck">Show Planck Data</Label>
            </div>
            <div className="flex items-center space-x-2">
              <Switch
                id="show-agdef"
                checked={showAGDEF}
                onCheckedChange={setShowAGDEF}
              />
              <Label htmlFor="show-agdef">Show AGDEF Prediction</Label>
            </div>
            <div className="flex items-center space-x-2">
              <Switch
                id="show-isw"
                checked={showISW}
                onCheckedChange={setShowISW}
              />
              <Label htmlFor="show-isw">Show ISW Effect</Label>
            </div>
          </div>

          <div className="space-y-2">
            <Label>AGDEF Strength: {agdefStrength.toFixed(2)}</Label>
            <Slider
              value={[agdefStrength]}
              onValueChange={(value) => setAgdefStrength(value[0])}
              min={0}
              max={1}
              step={0.01}
            />
          </div>

          <div className="h-[400px]">
            <Line data={chartData} options={chartOptions} />
          </div>

          <div className="text-sm text-muted-foreground">
            <p>
              The Integrated Sachs-Wolfe (ISW) effect is enhanced in the AGDEF
              model due to the non-local nature of the anti-gravity field. This
              leads to stronger correlations between CMB temperature fluctuations
              and large-scale structure at low multipoles (ℓ &lt; 30).
            </p>
          </div>
        </div>
      </CardContent>
    </Card>
  )
} 