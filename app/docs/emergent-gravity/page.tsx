'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";

export default function EmergentGravityPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Emergent Gravity (Verlinde)</h1>
        <p className="text-white/80 text-lg mb-8">
          Erik Verlinde's theory proposes that gravity is not a fundamental force but emerges from quantum entanglement,
          potentially explaining dark matter phenomena without the need for new particles.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="quantum" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Quantum Origins</TabsTrigger>
            <TabsTrigger value="predictions" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Predictions</TabsTrigger>
            <TabsTrigger value="status" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Current Status</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Emergent Gravity Theory</CardTitle>
                <CardDescription className="text-white/70">
                  Gravity as a consequence of information and entropy
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  In 2010, Dutch theoretical physicist Erik Verlinde proposed that gravity is not a fundamental force 
                  but rather an emergent phenomenon arising from changes in entropy. This approach, known as entropic gravity, 
                  suggests that gravity is a statistical tendency toward greater entropy, similar to how temperature emerges 
                  from the collective motion of particles.
                </p>
                <p>
                  Verlinde expanded on this concept in 2016 with his theory of Emergent Gravity, which specifically addresses 
                  dark matter phenomena. The theory proposes that what we perceive as dark matter is actually a manifestation 
                  of the emergent nature of gravity, particularly in regions where space has high entropy associated with quantum 
                  entanglement.
                </p>
                <div className="my-6 bg-zinc-900/50 p-4 rounded-md">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Key Concepts</h3>
                  <ul className="list-disc pl-6 space-y-2">
                    <li>Gravity emerges from quantum entanglement entropy in spacetime</li>
                    <li>Dark matter effects are explained as a volume law contribution to entropy</li>
                    <li>Apparent dark matter is a consequence of the displacement of quantum information</li>
                    <li>The theory connects to holographic principles and AdS/CFT correspondence</li>
                  </ul>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="quantum" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Quantum Entanglement Origins</CardTitle>
                <CardDescription className="text-white/70">
                  How quantum information relates to gravity
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Verlinde's theory builds on the insight that spacetime and gravity might emerge from quantum entanglement.
                  In his framework, the distribution of quantum information in space leads to what we interpret as gravitational effects.
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Entanglement Entropy and Gravity</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="S_{EE} \propto \frac{A}{4G\hbar}" />
                    </div>
                    <p className="mt-2">
                      The entanglement entropy (<InlineMath math="S_{EE}" />) is proportional to the area of the boundary, 
                      connecting to the Bekenstein-Hawking entropy of black holes, suggesting a deep relationship between 
                      quantum information and spacetime geometry.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Volume Law vs. Area Law</h3>
                    <p>
                      In conventional quantum field theory, entanglement entropy follows an "area law," scaling with the area of a region's boundary.
                      Verlinde proposes that in de Sitter space (like our universe), there's an additional "volume law" contribution
                      to the entropy due to dark energy, which manifests as apparent dark matter.
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="S_{volume} \propto V\cdot \rho_{DE}" />
                    </div>
                    <p className="mt-2">
                      This volume contribution to entropy creates an additional gravitational effect that mimics dark matter
                      without requiring new particles.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="predictions" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Observable Predictions</CardTitle>
                <CardDescription className="text-white/70">
                  How Emergent Gravity differs from ΛCDM
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Verlinde's Emergent Gravity makes several testable predictions that differ from the standard ΛCDM cosmology
                  with particle dark matter:
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Galaxy Rotation Curves</h3>
                    <p>
                      The theory predicts gravitational effects that closely resemble Modified Newtonian Dynamics (MOND) in galaxies:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="a(r) \approx \sqrt{a_0 \cdot a_N(r)}" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="a(r)" /> is the actual acceleration, <InlineMath math="a_N(r)" /> is the Newtonian acceleration, 
                      and <InlineMath math="a_0" /> is a constant related to the Hubble scale. This naturally explains flat rotation curves 
                      without dark matter particles.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Computational Implementation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`import numpy as np
import matplotlib.pyplot as plt

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
    # (simplified implementation of Verlinde's equations)
    a_DM = np.sqrt(a0 * a_N)
    
    # Total observed acceleration
    a_total = a_N + a_DM
    
    return a_total

# Example: Calculate for a galaxy
G = 6.67e-11  # Gravitational constant
M_galaxy = 1e10 * 2e30  # 10 billion solar masses in kg

# Radii from 1 to 30 kpc
r_values = np.linspace(1, 30, 100) * 3.086e19  # kpc to meters

# Calculate accelerations
a_newton = G * M_galaxy / r_values**2
a_total = emergent_gravity_acceleration(r_values, M_galaxy)

# Convert to velocities
v_newton = np.sqrt(r_values * a_newton)
v_total = np.sqrt(r_values * a_total)

# The emergent gravity model predicts flat rotation curves
# similar to observed galaxy behavior without dark matter`}
                      </code>
                    </pre>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Deviations from ΛCDM</h3>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>Predicts different galaxy-galaxy lensing signals compared to particle dark matter</li>
                      <li>Expects less structure formation at high redshifts</li>
                      <li>Predicts correlations between dark energy and apparent dark matter effects</li>
                      <li>May show distinctive signatures in gravitational wave propagation</li>
                    </ul>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="status" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Current Status</CardTitle>
                <CardDescription className="text-white/70">
                  Scientific reception and open questions
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Observational Tests</h3>
                    <p>
                      Several studies have attempted to test Verlinde's Emergent Gravity theory:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Studies of galaxy-galaxy lensing with KiDS survey data showed the theory may account for some observed effects</li>
                      <li>Analysis of dwarf galaxies suggests the theory correctly predicts their dynamics without dark matter</li>
                      <li>Challenges remain in fully explaining galaxy cluster dynamics and cosmic microwave background observations</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Theoretical Challenges</h3>
                    <p>
                      Major theoretical obstacles for Emergent Gravity include:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Developing a full relativistic formulation (current theory is mainly focused on the weak-field limit)</li>
                      <li>Reconciling with the successes of ΛCDM in explaining large-scale structure formation</li>
                      <li>Connecting more rigorously to quantum information theory</li>
                      <li>Addressing how emergent gravity behaves in strongly curved spacetime</li>
                    </ul>
                  </div>
                  
                  <div className="bg-zinc-900/50 p-4 rounded-md mt-6">
                    <h3 className="text-lg font-medium text-dark-pink mb-2">The Path Forward</h3>
                    <p>
                      Verlinde's Emergent Gravity represents a promising alternative approach to dark matter that connects quantum
                      information, gravity, and cosmology. While still in development, it offers fresh perspectives on fundamental
                      questions about spacetime and gravity. More theoretical work and observational tests are needed to fully
                      evaluate its viability compared to particle dark matter models.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
} 