'use client';

import { Card } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import 'katex/dist/katex.min.css';
import { InlineMath, BlockMath } from 'react-katex';
import { SimulationPlots } from "@/components/agdef/SimulationPlots";
import { ObservationPlots } from "@/components/agdef/ObservationPlots";

export default function AGDEFPage() {
  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-4xl font-bold mb-8">Anti-Gravity Dark Energy Field (AGDEF)</h1>
      
      <Tabs defaultValue="theory" className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="theory">Theory Overview</TabsTrigger>
          <TabsTrigger value="simulation">Simulation</TabsTrigger>
          <TabsTrigger value="observations">Observations</TabsTrigger>
          <TabsTrigger value="aging">Aging Theory</TabsTrigger>
        </TabsList>

        <TabsContent value="theory" className="mt-6">
          <Card className="p-6">
            <h2 className="text-2xl font-semibold mb-4">Theoretical Framework</h2>
            <p className="mb-4">
              The Anti-Gravity Dark Energy Field (AGDEF) theory proposes that dark energy emerges from 
              5-dimensional curvature projected into our 4D spacetime. This projection manifests as a 
              repulsive force that drives cosmic acceleration.
            </p>
            
            <div className="space-y-4">
              <div>
                <h3 className="text-xl font-medium mb-2">AGDEF Tensor</h3>
                <BlockMath math="\mathcal{A}_{MN} = \begin{pmatrix} A_{\mu\nu} & A_{\mu 5} \\ A_{5\nu} & A_{55} \end{pmatrix}" />
              </div>

              <div>
                <h3 className="text-xl font-medium mb-2">4D Projection</h3>
                <BlockMath math="P^\top \mathcal{A} P = A_{\mu\nu}" />
              </div>

              <div>
                <h3 className="text-xl font-medium mb-2">Modified Friedmann Equation</h3>
                <BlockMath math="H^2 = \frac{8\pi G}{3}\rho + \frac{\Lambda}{3} + \frac{\kappa}{3}\mathcal{A}_{55}" />
              </div>

              <div>
                <h3 className="text-xl font-medium mb-2">Time Evolution</h3>
                <BlockMath math="\frac{d\mathcal{A}_{55}}{dt} = -\frac{\kappa}{2}\mathcal{A}_{55}^2" />
              </div>
            </div>
          </Card>
        </TabsContent>

        <TabsContent value="simulation" className="mt-6">
          <Card className="p-6">
            <h2 className="text-2xl font-semibold mb-4">Haskell Simulation Results</h2>
            <p className="mb-4">
              The following plots show the simulated distance modulus and luminosity distance 
              as functions of redshift, comparing AGDEF predictions with ΛCDM.
            </p>
            <SimulationPlots />
          </Card>
        </TabsContent>

        <TabsContent value="observations" className="mt-6">
          <Card className="p-6">
            <h2 className="text-2xl font-semibold mb-4">Observational Data Comparison</h2>
            <p className="mb-4">
              Comparison of AGDEF predictions with Pantheon+ supernova data and 
              Planck CMB Integrated Sachs-Wolfe effect measurements.
            </p>
            <ObservationPlots />
          </Card>
        </TabsContent>

        <TabsContent value="aging" className="mt-6">
          <Card className="p-6">
            <h2 className="text-2xl font-semibold mb-4">Can AGDEF Explain Aging?</h2>
            <p className="mb-4">
              The curvature of spacetime in AGDEF theory may influence biological processes 
              through modifications to the local time evolution. The repulsive nature of the 
              dark energy field could potentially affect cellular processes and contribute to 
              the aging mechanism.
            </p>
            <div className="space-y-4">
              <div>
                <h3 className="text-xl font-medium mb-2">Biological Entropy Equation</h3>
                <BlockMath math="\frac{dS}{dt} = \frac{dS_0}{dt} \exp\left(-\frac{\kappa}{2}\mathcal{A}_{55}t\right)" />
              </div>
              <div>
                <h3 className="text-xl font-medium mb-2">Cellular Time Dilation</h3>
                <BlockMath math="\frac{d\tau}{dt} = \sqrt{1 - \frac{\kappa}{3}\mathcal{A}_{55}r^2}" />
              </div>
              <p className="text-sm text-gray-600">
                For more detailed analysis of AGDEF's implications for longevity, visit our 
                <a href="/longevity" className="text-blue-600 hover:underline ml-1">
                  dedicated longevity research page
                </a>.
              </p>
            </div>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
} 