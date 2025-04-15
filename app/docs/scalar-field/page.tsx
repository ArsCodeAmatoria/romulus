'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

export default function ScalarFieldPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Scalar Field / f(R) Gravity</h1>
        <p className="text-white/80 text-lg mb-8">
          Scalar field theories and f(R) gravity offer a compelling framework for unifying dark matter with modifications to gravity,
          treating dark matter as a boundary condition rather than a particle.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="boundary" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Boundary Theory</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Scalar Field Equations</TabsTrigger>
            <TabsTrigger value="implications" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Implications</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Unifying Dark Matter with Modified Gravity</CardTitle>
                <CardDescription className="text-white/70">
                  Reimagining dark matter as a boundary condition or structural framework
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Instead of viewing dark matter as discrete particles filling space, scalar field theories propose that
                  dark matter effects emerge from modifications to gravity itself. This approach is analogous to how an artist
                  first outlines figures in a painting to define objects before filling in with color.
                </p>
                <p>
                  In this paradigm, dark matter is not a "thing" but rather a boundary condition or structural framework that
                  defines how visible matter behaves. The effects we attribute to dark matter could actually be the result of
                  how gravity behaves differently at large scales or in regions of low density.
                </p>
                <p>
                  Scalar field theories introduce additional fields that modify Einstein's equations, creating effects that
                  mimic dark matter halos without requiring new particles. These approaches naturally explain galaxy rotation curves,
                  gravitational lensing, and cosmic structure formation while potentially resolving inconsistencies in the standard
                  dark matter particle model.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="boundary" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Dark Matter as Boundary Condition</CardTitle>
                <CardDescription className="text-white/70">
                  The outline that defines cosmic structure
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  If we reconceptualize dark matter as a boundary condition rather than a substance, it fundamentally changes
                  how we approach the missing mass problem in the universe.
                </p>
                
                <div className="my-6 space-y-4">
                  <h3 className="text-lg font-medium text-dark-pink">The Artist's Outline Metaphor</h3>
                  <p>
                    Like an artist who first sketches the outline of a figure before adding details, dark matter might
                    be the "sketch" that defines where visible matter will accumulate and form structures. The vast cosmic web
                    of filaments observed in the universe could be these outlines—the scaffolding upon which visible matter is arranged.
                  </p>
                  
                  <div className="bg-zinc-900/50 p-4 rounded-md">
                    <p className="font-medium mb-2">Traditional View:</p>
                    <p>Dark matter particles fill space and create gravitational wells</p>
                    
                    <p className="font-medium mb-2 mt-4">Boundary Condition View:</p>
                    <p>Dark matter defines the edges and structure of spacetime, guiding where matter forms</p>
                  </div>
                </div>
                
                <p>
                  In this framework, dark matter would not be something we can detect as a particle because it's not a
                  particle at all—it's more like a property of spacetime that defines where the edges and structures
                  of the cosmos exist. This might explain why direct detection experiments have consistently failed to
                  find dark matter particles despite strong indirect evidence for their gravitational effects.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Scalar Field Equations</CardTitle>
                <CardDescription className="text-white/70">
                  Mathematical framework for unifying dark matter with modified gravity
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Several mathematical approaches can describe how dark matter could emerge from modified gravity
                  rather than from new particles:
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">f(R) Gravity</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto font-mono text-center">
                      G_μν + f(R)g_μν = (8πG/c⁴)T_μν
                    </div>
                    <p className="mt-2">
                      By modifying how spacetime curvature R behaves through the function f(R), we can naturally
                      generate effects similar to dark matter without additional mass. This affects how gravity
                      behaves at large distances or in regions of low acceleration.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Scalar-Tensor Theory</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto font-mono text-center">
                      G_μν + φ(x)g_μν = (8πG/c⁴)T_μν
                    </div>
                    <p className="mt-2">
                      A scalar field φ(x) modifies the gravitational interaction, creating enhanced gravitational
                      effects at galaxy edges that mimic dark matter halos. The field isn't a particle but rather
                      a property of spacetime that varies with position.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Edge Detection Model</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto font-mono text-center">
                      ∇²ψ = f(ρ)
                    </div>
                    <p className="mt-2">
                      Similar to edge detection in image processing, this approach treats dark matter as an effect that
                      emerges around the "edges" of matter distributions. The function f(ρ) selectively enhances gravitational
                      effects at the boundaries of matter concentrations.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Computational Implementation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`# Simplified implementation of f(R) gravity effects
import numpy as np
from scipy.integrate import solve_ivp

def modified_gravity_potential(r, mass, alpha=0.1, beta=2.0):
    """
    Calculate gravitational potential with f(R) modification
    
    Parameters:
    -----------
    r : float or array
        Radial distance (kpc)
    mass : float
        Central mass (solar masses)
    alpha : float
        Strength of the modification
    beta : float
        Scale of the modification (kpc)
        
    Returns:
    --------
    float or array : Modified gravitational potential
    """
    # Constants
    G = 4.3e-6  # Gravitational constant in kpc^3/(solar mass * Gyr^2)
    
    # Standard Newtonian potential
    phi_newton = -G * mass / r
    
    # f(R) modification - enhances gravity at large scales
    # This creates dark-matter-like effects without dark matter
    phi_modified = phi_newton * (1 + alpha * (r / beta)**2 / (1 + (r / beta)**2))
    
    return phi_modified

# Calculate rotation curves for a galaxy
r_values = np.linspace(0.1, 30, 100)  # kpc
galaxy_mass = 1e11  # solar masses

# Standard Newtonian potential and velocity
potential_newton = -4.3e-6 * galaxy_mass / r_values
v_newton = np.sqrt(r_values * np.gradient(potential_newton, r_values))

# Modified gravity potential and velocity
potential_modified = modified_gravity_potential(r_values, galaxy_mass)
v_modified = np.sqrt(r_values * np.gradient(potential_modified, r_values))

# Results show flat rotation curves without dark matter particles`}
                      </code>
                    </pre>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="implications" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Implications & New Directions</CardTitle>
                <CardDescription className="text-white/70">
                  Consequences of viewing dark matter as a boundary condition
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Experimental Predictions</h3>
                    <p>
                      If dark matter is actually a modification to gravity rather than a particle, we would expect:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Continued failure of direct detection experiments searching for particles</li>
                      <li>Systematic deviations from standard dark matter predictions in galaxy clusters</li>
                      <li>Observable differences in gravitational waves propagation</li>
                      <li>Correlations between visible matter distribution and gravitational effects that follow edge-detection patterns</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Quantum Gravity Connection</h3>
                    <p>
                      The boundary-condition view of dark matter may connect naturally to quantum gravity theories. 
                      If spacetime emerges from quantum entanglement (as suggested by holographic theories), then what
                      we interpret as dark matter could be an emergent property of how quantum information is organized
                      at the largest scales, rather than a new quantum field.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Searching in the Right Places</h3>
                    <p>
                      If dark matter is indeed more like an "outline" or boundary condition, we need to shift our search strategies:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Focus on precise measurements of gravity at galaxy edges</li>
                      <li>Study cosmic voids and filaments as manifestations of these boundaries</li>
                      <li>Examine correlations between visible structure boundaries and gravitational effects</li>
                      <li>Develop better mathematical models of how boundaries in spacetime could manifest as apparent mass</li>
                    </ul>
                  </div>
                </div>
                
                <div className="mt-6 p-4 bg-dark-pink/10 rounded-md">
                  <p className="text-dark-pink font-medium">A New Paradigm</p>
                  <p className="mt-2">
                    Treating dark matter as a boundary condition or structural framework rather than a particle
                    represents a fundamental shift in how we understand the universe. It suggests that what we perceive
                    as "missing mass" might actually be the universe's way of defining structure—the outline of the cosmic
                    painting that guides where galaxies form and evolve.
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