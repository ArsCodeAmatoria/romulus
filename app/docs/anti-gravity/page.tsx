'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import SpacetimeCurvature from "@/components/interactive/SpacetimeCurvature";

export default function AntiGravityPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Dark Matter as Anti-Gravity</h1>
        <p className="text-white/80 text-lg mb-8">
          What if dark matter isn't matter at all, but a repulsive gravitational effect that creates negative curvature 
          in spacetime, indirectly pushing matter together by sculpting spacetime differently?
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Key Equations</TabsTrigger>
            <TabsTrigger value="models" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Physical Models</TabsTrigger>
            <TabsTrigger value="implications" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Implications</TabsTrigger>
            <TabsTrigger value="simulation" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Simulation</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Dark Matter as Anti-Gravity</CardTitle>
                <CardDescription className="text-white/70">
                  Reimagining dark matter as a repulsive gravitational effect
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Instead of being "extra mass" attracting things (as the current model assumes), this theory proposes 
                  that dark matter is not matter at all, but a repulsive gravitational effect that creates negative 
                  curvature in spacetime in certain regions.
                </p>
                <p>
                  In this framework, dark matter isn't pulling galaxies together — it's shaping spacetime so that 
                  matter clumps without needing extra mass. This represents a fundamental shift in how we conceptualize 
                  the dark matter problem.
                </p>
                <p>
                  By introducing regions of negative curvature in spacetime, this theory can potentially explain the 
                  observed galaxy rotation curves, gravitational lensing, and large-scale structure of the universe 
                  without requiring invisible particles.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Key Equations</CardTitle>
                <CardDescription className="text-white/70">
                  Mathematical framework for anti-gravity as dark matter
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <div className="space-y-6">
                  <p>
                    Normally, General Relativity uses positive energy-momentum to curve spacetime inward:
                  </p>
                  <div>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="T_{\mu\nu}" /> is positive for normal matter/energy (mass curves spacetime inward).
                      But negative energy density would curve spacetime outward — an anti-gravity effect.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Modified Field Equations</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} = \frac{8\pi G}{c^4}(T_{\mu\nu} - \chi_{\mu\nu})" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="\chi_{\mu\nu}" /> is a tensor representing regions of negative gravitational curvature.
                      This would repel spacetime outward in some regions, causing contraction in others, leading to galaxy 
                      clustering and gravitational lensing without extra mass.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Toy Equation Example</h3>
                    <p>
                      We can model the anti-gravity field <InlineMath math="\phi(x)" /> as satisfying a Poisson-like equation:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\nabla^2\phi = -4\pi G\rho_{dark}" />
                    </div>
                    <p className="mt-2">
                      Notice the negative sign:
                    </p>
                    <ul className="list-disc pl-6 space-y-1">
                      <li>Ordinary mass <InlineMath math="\rho" /> gives attractive gravity: <InlineMath math="+4\pi G\rho" />.</li>
                      <li>Dark matter (as anti-gravity) gives repulsive gravity: <InlineMath math="-4\pi G\rho_{dark}" />.</li>
                    </ul>
                    <p className="mt-2">
                      The total gravitational potential becomes:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\Phi_{total} = \Phi_{visible} + \phi" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="\Phi_{visible}" /> is the normal gravitational potential from stars and galaxies.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="models" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Physical Models</CardTitle>
                <CardDescription className="text-white/70">
                  Potential physical interpretations of anti-gravity fields
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  There are several possible physical interpretations for how an anti-gravity field could manifest:
                </p>
                
                <div className="mt-4">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Vacuum Pressure Model</h3>
                  <p>
                    Some regions of space could have negative vacuum pressure that acts like an anti-gravity lens. 
                    This would be similar to a localized "cosmological constant" that pushes outward rather than pulls inward.
                  </p>
                  <p className="mt-2">
                    The negative pressure regions would create a structured pattern of spacetime curvature that shapes 
                    how matter clusters at large scales, without requiring additional mass.
                  </p>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Exotic Field Model</h3>
                  <p>
                    A new scalar or tensor field (represented as <InlineMath math="\phi(x)" />) could act to counterbalance 
                    normal gravity in a structured way. This would be similar to how inflation worked in the early universe, 
                    but weaker and with a different spatial distribution.
                  </p>
                  <p className="mt-2">
                    The field would have specific coupling properties to create the observed large-scale structure without 
                    affecting gravity at solar system scales, where General Relativity is well-tested.
                  </p>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Observational Evidence</h3>
                  <p>
                    This model would explain key observations that are currently attributed to dark matter:
                  </p>
                  <ul className="list-disc pl-6 space-y-2 mt-2">
                    <li>
                      <span className="text-dark-pink font-medium">Galaxy Rotation Curves:</span> Instead of extra matter 
                      pulling stars around faster, localized anti-gravity fields could create deeper spacetime wells indirectly 
                      pulling visible matter into faster orbits.
                    </li>
                    <li>
                      <span className="text-dark-pink font-medium">Gravitational Lensing:</span> The bending of light could 
                      happen because of altered spacetime curvature, not just from added mass.
                    </li>
                    <li>
                      <span className="text-dark-pink font-medium">Large Scale Structure:</span> Anti-gravity fields could 
                      "outline" the cosmic web structure by repelling normal matter into filaments and voids.
                    </li>
                  </ul>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="implications" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Implications</CardTitle>
                <CardDescription className="text-white/70">
                  Broader consequences of the anti-gravity model
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  If dark matter is indeed a manifestation of anti-gravity effects rather than additional mass, 
                  this would have profound implications for our understanding of physics:
                </p>
                
                <div className="mt-4">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Unification with Dark Energy</h3>
                  <p>
                    This approach would potentially unify dark matter with dark energy — as both would be forms of 
                    modified spacetime curvature rather than distinct substances. This parsimony is theoretically 
                    attractive, replacing two mysterious components with a single modification to gravity.
                  </p>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Richer Gravitational Physics</h3>
                  <p>
                    It means gravity would be a richer and more complex phenomenon than just an attractive force due to mass. 
                    Gravity could have both attractive and repulsive components depending on the underlying physics that 
                    generates the spacetime curvature.
                  </p>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Testable Predictions</h3>
                  <p>
                    The anti-gravity model would make distinct predictions that could differentiate it from 
                    particle dark matter models:
                  </p>
                  <ul className="list-disc pl-6 space-y-2 mt-2">
                    <li>Different galaxy merger dynamics due to the distribution of the anti-gravity field</li>
                    <li>Unique patterns in the cosmic microwave background polarization</li>
                    <li>Specific signatures in gravitational wave signals from galaxy clusters</li>
                    <li>Different predictions for structure formation in the early universe</li>
                  </ul>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Challenge to Current Paradigm</h3>
                  <p>
                    The anti-gravity approach represents a fundamental shift in our understanding of cosmic structure. 
                    Rather than searching for dark matter particles, it suggests we need to revise our understanding of 
                    how spacetime itself behaves at cosmic scales.
                  </p>
                  <p className="mt-2">
                    This would require a major conceptual shift in theoretical physics, but it has the potential to 
                    resolve persistent challenges in cosmology while opening new avenues for research.
                  </p>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="simulation" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Simulation</CardTitle>
                <CardDescription className="text-white/70">
                  Interactive demonstration of anti-gravity spacetime effects
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <p className="mb-4">
                  This interactive visualization demonstrates how anti-gravity fields might curve spacetime differently 
                  than traditional mass. The grid represents spacetime being deformed by both normal gravity (pulling inward) 
                  and anti-gravity effects (pushing outward in certain regions).
                </p>
                <div className="bg-zinc-900/50 p-4 rounded-md">
                  <SpacetimeCurvature />
                </div>
                <p className="mt-4">
                  Observe how the interplay between normal gravity and anti-gravity effects creates a complex curvature pattern. 
                  This unique spacetime geometry could explain galaxy rotation curves and cosmic structure without requiring 
                  dark matter particles. The simulation demonstrates how negative curvature regions indirectly influence the 
                  distribution of matter by shaping the overall gravitational landscape.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
} 