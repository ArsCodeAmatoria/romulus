'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import { SimulationPlots } from "@/components/agdef/SimulationPlots";
import { ObservationPlots } from "@/components/agdef/ObservationPlots";

export default function AGDEFPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Anti-Gravity Dark Energy Field (AGDEF)</h1>
        <p className="text-white/80 text-lg mb-8">
          A novel theory proposing that dark energy emerges from anti-gravitational tension in higher-dimensional spacetime, 
          manifesting as a projection of extrinsic curvature from a higher-dimensional matrix field into our 3D+1 spacetime.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Key Equations</TabsTrigger>
            <TabsTrigger value="simulation" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Simulation</TabsTrigger>
            <TabsTrigger value="observations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Observations</TabsTrigger>
            <TabsTrigger value="aging" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Aging Theory</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Theoretical Framework</CardTitle>
                <CardDescription className="text-white/70">
                  Higher-dimensional anti-gravity as dark energy
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  The Anti-Gravity Dark Energy Field (AGDEF) theory proposes that dark energy emerges from 
                  anti-gravitational tension in higher-dimensional spacetime. Rather than being a uniform 
                  repulsive force, it's a projection from a higher-dimensional matrix field that manifests 
                  as repulsive curvature in our 3D+1 spacetime.
                </p>
                <p>
                  Our observable universe is modeled as a 3D brane embedded in a higher-dimensional space 
                  (4D+1). The dark energy we observe is a byproduct of the extrinsic curvature from this 
                  brane's embedding in higher dimensions.
                </p>
                <p>
                  Anti-gravity is defined through negative eigenvalues of a metric tensor extended to higher 
                  dimensions, behaving inversely to General Relativity's stress-energy tensor: instead of 
                  curving spacetime inward (like mass), it curves it outward.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Key Equations</CardTitle>
                <CardDescription className="text-white/70">
                  Mathematical framework for AGDEF theory
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Metric Perturbation</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} = \eta_{\mu\nu} + h_{\mu\nu}" />
                    </div>
                    <p className="mt-2">
                      The metric tensor <InlineMath math="G_{\mu\nu}" /> is expressed as a perturbation 
                      <InlineMath math="h_{\mu\nu}" /> of the Minkowski metric <InlineMath math="\eta_{\mu\nu}" />.
                    </p>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Anti-Gravity Matrix</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="A = -k \cdot M" />
                    </div>
                    <p className="mt-2">
                      The anti-gravity matrix <InlineMath math="A" /> is defined in terms of the mass tensor field 
                      <InlineMath math="M" /> and coupling constant <InlineMath math="k \in \mathbb{R}^+" />.
                    </p>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Eigen-decomposition</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="A = PDP^{-1}, \text{ where } D = \text{diag}(-\lambda_1, -\lambda_2, \ldots)" />
                    </div>
                    <p className="mt-2">
                      The negative eigenvalues <InlineMath math="-\lambda_i" /> drive the repulsive behavior 
                      in spacetime.
                    </p>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Matrix Evolution</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\frac{dX}{dt} = [H, X]" />
                    </div>
                    <p className="mt-2">
                      The state matrix <InlineMath math="X(t) \in \mathbb{R}^{n \times n}" /> evolves according 
                      to a Hamiltonian-like generator <InlineMath math="H" />, mimicking quantum mechanical evolution.
                    </p>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Dimensional Projection</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="P: \mathbb{R}^5 \to \mathbb{R}^4, x' = Px" />
                    </div>
                    <p className="mt-2">
                      The projection operator <InlineMath math="P" /> maps from 5D to 4D spacetime, with 
                      <InlineMath math="x'" /> representing the projected coordinates.
                    </p>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Dark Energy Expression</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="E_{\text{dark}}(x) = \|PAx\|^2" />
                    </div>
                    <p className="mt-2">
                      The apparent dark energy density is given by the squared norm of the projected 
                      anti-gravity matrix acting on the spacetime coordinates.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="simulation" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Haskell Simulation Results</CardTitle>
                <CardDescription className="text-white/70">
                  Simulated distance modulus and luminosity distance predictions
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <p className="mb-4">
                  The following plots show the simulated distance modulus and luminosity distance 
                  as functions of redshift, comparing AGDEF predictions with ΛCDM.
                </p>
                <SimulationPlots />
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="observations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Observational Data Comparison</CardTitle>
                <CardDescription className="text-white/70">
                  Comparison with Pantheon+ and Planck CMB data
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <p className="mb-4">
                  Comparison of AGDEF predictions with Pantheon+ supernova data and 
                  Planck CMB Integrated Sachs-Wolfe effect measurements.
                </p>
                <ObservationPlots />
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="aging" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Can AGDEF Explain Aging?</CardTitle>
                <CardDescription className="text-white/70">
                  Implications for biological processes and longevity
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  The curvature of spacetime in AGDEF theory may influence biological processes 
                  through modifications to the local time evolution. The repulsive nature of the 
                  dark energy field could potentially affect cellular processes and contribute to 
                  the aging mechanism.
                </p>
                
                <div className="mt-4">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Biological Entropy Equation</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\frac{dS}{dt} = \frac{dS_0}{dt} \exp\left(-\frac{\kappa}{2}\mathcal{A}_{55}t\right)" />
                  </div>
                  <p className="mt-2">
                    The AGDEF field modifies the rate of biological entropy production, potentially 
                    explaining the observed patterns of aging.
                  </p>
                </div>

                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Cellular Time Dilation</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\frac{d\tau}{dt} = \sqrt{1 - \frac{\kappa}{3}\mathcal{A}_{55}r^2}" />
                  </div>
                  <p className="mt-2">
                    Local spacetime geometry affects cellular processes through time dilation effects, 
                    potentially influencing biological aging.
                  </p>
                </div>

                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Future Research</h3>
                  <p>
                    For more detailed analysis of AGDEF's implications for longevity, visit our 
                    <a href="/longevity" className="text-dark-pink hover:underline ml-1">
                      dedicated longevity research page
                    </a>.
                  </p>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
} 