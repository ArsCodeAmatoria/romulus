'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import TensorNetwork from "@/components/visualization/TensorNetwork";

export default function QuantumGravityPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">Quantum Gravity + Holographic Principle</h1>
        <p className="text-white/80 text-lg mb-8">
          Exploring how quantum gravity theories and the holographic principle might offer new perspectives on dark matter,
          suggesting it could emerge from information encoding at the boundaries of spacetime.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="holography" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Holographic Principle</TabsTrigger>
            <TabsTrigger value="entanglement" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Quantum Entanglement</TabsTrigger>
            <TabsTrigger value="dark-matter" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Dark Matter Connection</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Quantum Gravity Approaches</CardTitle>
                <CardDescription className="text-white/70">
                  Unifying quantum mechanics and gravity
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Quantum gravity represents the ongoing effort to reconcile quantum mechanics with general relativity,
                  creating a unified theory that works at all scales. While no complete theory exists yet, several 
                  approaches offer compelling frameworks that might reshape our understanding of dark matter.
                </p>
                
                <div className="my-6 bg-zinc-900/50 p-4 rounded-md">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Major Quantum Gravity Approaches</h3>
                  <ul className="list-disc pl-6 space-y-2">
                    <li>
                      <strong>String Theory:</strong> Replaces point particles with tiny vibrating strings, requiring extra 
                      dimensions and potentially containing particle candidates for dark matter
                    </li>
                    <li>
                      <strong>Loop Quantum Gravity:</strong> Quantizes spacetime itself into discrete units at the Planck scale, 
                      suggesting gravity emerges from more fundamental quantum geometry
                    </li>
                    <li>
                      <strong>Causal Set Theory:</strong> Models spacetime as a discrete causal structure, with gravity emerging 
                      from the causal relationships between events
                    </li>
                    <li>
                      <strong>Asymptotic Safety:</strong> Proposes that gravity becomes scale-invariant at high energies, avoiding 
                      the infinities that plague quantum field theory
                    </li>
                    <li>
                      <strong>Emergent Gravity:</strong> Suggests that gravity is not fundamental but emerges from more basic quantum 
                      phenomena like entanglement entropy
                    </li>
                  </ul>
                </div>
                
                <p>
                  What makes quantum gravity particularly relevant to dark matter is the possibility that what we perceive as missing 
                  mass might actually be a manifestation of quantum gravitational effects. Rather than being composed of particles, 
                  dark matter could represent a misunderstanding of how gravity behaves when quantum effects become significant.
                </p>
                
                <p>
                  The holographic principle, which emerged from black hole thermodynamics and string theory, suggests that the 
                  information in a volume of space can be encoded on its boundary. This revolutionary concept hints that three-dimensional 
                  phenomena like dark matter might be projections of information encoded on two-dimensional boundaries.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="holography" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">The Holographic Principle</CardTitle>
                <CardDescription className="text-white/70">
                  Reality as a projection of boundary information
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  The holographic principle posits that the entire information content of a region of space can be represented 
                  by information on the boundary of that region. This revolutionary idea emerged from the study of black hole 
                  thermodynamics by Jacob Bekenstein and Stephen Hawking in the 1970s.
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Black Hole Entropy</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="S_{BH} = \frac{k_B c^3 A}{4G\hbar}" />
                    </div>
                    <p className="mt-2">
                      The Bekenstein-Hawking entropy formula reveals that a black hole's entropy is proportional to its 
                      surface area (<InlineMath math="A" />), not its volume. This suggests that the maximum information 
                      content of any region of space is limited by its boundary area, not its volume.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">The Holographic Bound</h3>
                    <p>
                      The holographic bound states that the information contained in a region of space is limited by its boundary area:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="I \leq \frac{A}{4\ell_P^2}" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="I" /> is the information content in bits, <InlineMath math="A" /> is the area of the boundary, 
                      and <InlineMath math="\ell_P" /> is the Planck length. This means a region of space can contain at most one bit of 
                      information per four Planck areas on its boundary.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">AdS/CFT Correspondence</h3>
                    <p>
                      The most concrete realization of the holographic principle is the Anti-de Sitter/Conformal Field Theory (AdS/CFT) 
                      correspondence, proposed by Juan Maldacena in 1997. This establishes a duality between:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>A gravitational theory in (n+1)-dimensional Anti-de Sitter space</li>
                      <li>A quantum field theory without gravity on the n-dimensional boundary of that space</li>
                    </ul>
                    <p className="mt-2">
                      This suggests that gravity in a higher-dimensional space might be equivalent to non-gravitational physics 
                      in a lower-dimensional space.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Implications for Cosmology</h3>
                    <p>
                      The holographic principle has profound implications for our understanding of space, time, and gravity:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Spacetime might be emergent rather than fundamental</li>
                      <li>The universe could be a holographic projection of information encoded on a distant surface</li>
                      <li>Quantum gravity effects might be understood through equivalent non-gravitational physics</li>
                      <li>Our three-dimensional perception could be an emergent property of more fundamental two-dimensional physics</li>
                    </ul>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="entanglement" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Quantum Entanglement and Spacetime</CardTitle>
                <CardDescription className="text-white/70">
                  How quantum information shapes the fabric of reality
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Recent developments suggest that quantum entanglement might be the key to understanding how spacetime emerges 
                  and how gravity works at the quantum level. This perspective, sometimes called "It from Qubit," proposes that 
                  the fabric of spacetime emerges from patterns of quantum entanglement.
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Entanglement Entropy and Area</h3>
                    <p>
                      When a quantum system is divided into two regions (A and B), the entanglement entropy quantifies how much 
                      information in region A depends on region B. Remarkably, this entropy is proportional to the area of the 
                      boundary between the regions:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="S_{EE} \propto \frac{\text{Area of boundary}}{\text{Area per entangled bit}}" />
                    </div>
                    <p className="mt-2">
                      This "area law" for entanglement entropy mirrors the Bekenstein-Hawking entropy formula for black holes, 
                      suggesting a deep connection between quantum information and spacetime geometry.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">ER=EPR Conjecture</h3>
                    <p>
                      In 2013, Juan Maldacena and Leonard Susskind proposed the "ER=EPR" conjecture, suggesting that:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>
                        <strong>ER:</strong> Einstein-Rosen bridges (wormholes connecting distant regions of spacetime)
                      </li>
                      <li>
                        <strong>EPR:</strong> Einstein-Podolsky-Rosen pairs (quantum entangled particles)
                      </li>
                    </ul>
                    <p className="mt-2">
                      This conjecture proposes that these phenomena are different perspectives on the same physics, implying 
                      that quantum entanglement creates microscopic wormholes connecting entangled particles.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Spacetime from Entanglement</h3>
                    <p>
                      Building on these ideas, researchers like Mark Van Raamsdonk have proposed that spacetime itself is "woven" 
                      from quantum entanglement. In this picture:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Regions of space are connected because their underlying quantum degrees of freedom are entangled</li>
                      <li>The strength of entanglement determines the distance between regions</li>
                      <li>Gravity emerges as a consequence of how entanglement changes</li>
                      <li>Disentangling quantum systems would be equivalent to tearing the fabric of spacetime</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Tensor Networks and Spacetime</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md">
                      <TensorNetwork />
                    </div>
                    <p className="mt-4">
                      Tensor networks—mathematical structures used to represent highly entangled quantum systems—have been shown 
                      to naturally encode hyperbolic geometry, the same geometry found in Anti-de Sitter space. This suggests that 
                      the mathematical structure of entangled quantum states naturally gives rise to emergent spacetime geometry.
                    </p>
                    <p className="mt-2 text-sm text-white/60">
                      Note: The visualization above shows a tensor network in hyperbolic space. Each node represents a tensor (quantum information), 
                      and the connections represent entanglement between them. The hyperbolic structure naturally emerges from the pattern of 
                      quantum entanglement. Drag to rotate and zoom to explore the network.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="dark-matter" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Dark Matter as a Quantum Gravity Effect</CardTitle>
                <CardDescription className="text-white/70">
                  Reinterpreting missing mass through quantum information
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Holographic Dark Matter</h3>
                    <p>
                      If the universe operates according to holographic principles, dark matter might not be a physical substance 
                      at all but rather a manifestation of how information is encoded on cosmic boundaries. In this perspective:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>
                        The apparent mass discrepancy in galaxies could result from how information about the galaxy's mass 
                        is encoded on holographic boundaries
                      </li>
                      <li>
                        What appears as "missing mass" might be additional gravity emerging from boundary information that 
                        isn't directly observable as particles
                      </li>
                      <li>
                        The distribution of dark matter in halos around galaxies might reflect optimal information encoding 
                        at the boundaries of cosmic structures
                      </li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Entanglement as Dark Matter</h3>
                    <p>
                      If spacetime emerges from quantum entanglement, variations in entanglement patterns could create 
                      gravitational effects that we currently attribute to dark matter:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>
                        Certain entanglement structures might create additional gravitational attraction without 
                        corresponding observable matter
                      </li>
                      <li>
                        The cosmic web—the large-scale structure of dark matter in the universe—could reflect the underlying 
                        network of quantum entanglement
                      </li>
                      <li>
                        Galaxy formation and evolution might be guided by changes in entanglement patterns rather than 
                        the gravitational collapse of dark matter particles
                      </li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Quantum Gravity Corrections</h3>
                    <p>
                      Various approaches to quantum gravity predict corrections to Einstein's field equations that become 
                      significant at galactic scales:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="G_{\mu\nu} + \Lambda g_{\mu\nu} + \alpha Q_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="Q_{\mu\nu}" /> represents quantum gravitational corrections and <InlineMath math="\alpha" /> 
                      is a coupling constant. These corrections could mimic the gravitational effects of dark matter without 
                      requiring new particles.
                    </p>
                  </div>
                  
                  <div className="bg-zinc-900/50 p-4 rounded-md mt-6">
                    <h3 className="text-lg font-medium text-dark-pink mb-2">A New Research Direction</h3>
                    <p>
                      Approaching dark matter as a quantum gravitational phenomenon offers several advantages:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>
                        It could explain why dark matter particles have persistently evaded direct detection despite 
                        decades of increasingly sensitive experiments
                      </li>
                      <li>
                        It provides a more fundamental explanation for dark matter that connects to the deepest questions 
                        about the nature of spacetime
                      </li>
                      <li>
                        It aligns with historical patterns where apparent anomalies (like Mercury's orbit) were resolved not 
                        by adding new entities but by deepening our understanding of fundamental physics
                      </li>
                    </ul>
                    <p className="mt-4">
                      While still speculative, these approaches to dark matter represent a frontier where quantum information 
                      theory, holography, and gravitation converge. They suggest that what we call "dark matter" might ultimately 
                      be a window into the quantum nature of spacetime itself.
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