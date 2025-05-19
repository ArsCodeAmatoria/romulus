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
            <TabsTrigger value="postulates" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Postulates</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Key Equations</TabsTrigger>
            <TabsTrigger value="comparison" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">vs ΛCDM</TabsTrigger>
            <TabsTrigger value="implications" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Implications</TabsTrigger>
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

          <TabsContent value="postulates" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Core Postulates</CardTitle>
                <CardDescription className="text-white/70">
                  Fundamental principles of AGDEF theory
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-6">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Matrix-Valued Dark Energy</h3>
                  <p>
                    Dark energy is not a scalar field (like the cosmological constant), but rather a matrix-valued 
                    field that manifests as a repulsive force due to its negative eigenstructure.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Higher-Dimensional Origin</h3>
                  <p>
                    Mass-energy in higher dimensions induces anti-gravity curvature projected into our spacetime. 
                    Observable effects (expansion of space) are geometric projections from a 5D anti-gravity field 
                    to 4D spacetime.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Dark Matter as Curvature Shadow</h3>
                  <p>
                    Dark matter is not composed of particles but is the shadow of tensorial interactions that warp 
                    geodesics non-locally through higher-dimensional curvature.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Tesseract Brane Concept</h3>
                  <p>
                    The universe is proposed to be a 4D hypersurface (tesseract) with dynamic embedding curvature. 
                    The 4D brane geometry warps in the 5th dimension under anti-gravity pressure.
                  </p>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Mathematical Framework</CardTitle>
                <CardDescription className="text-white/70">
                  Core equations and mathematical structure
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-6">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">5D Anti-Gravity Field Tensor</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="A_{MN} = -\kappa T_{MN}" />
                  </div>
                  <p className="mt-2">
                    The anti-gravity tensor <InlineMath math="A_{MN} \in \mathbb{R}^{5 \times 5}" /> is defined in terms of 
                    the 5D stress-energy tensor <InlineMath math="T_{MN}" /> and anti-gravity constant 
                    <InlineMath math="\kappa" /> (analogous to <InlineMath math="8\pi G" />).
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">4D Projection</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="A_{\mu\nu} = P^\top A_{MN} P" />
                  </div>
                  <p className="mt-2">
                    The projection operator <InlineMath math="P: \mathbb{R}^5 \to \mathbb{R}^4" /> maps the 5D 
                    anti-gravity tensor into our 4D spacetime, where <InlineMath math="\mu,\nu = 0,1,2,3" /> 
                    are spacetime indices.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Modified Einstein Equation</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="R_{\mu\nu} - \frac{1}{2}Rg_{\mu\nu} = 8\pi G(T_{\mu\nu} + A_{\mu\nu})" />
                  </div>
                  <p className="mt-2">
                    The anti-gravity term <InlineMath math="A_{\mu\nu}" /> enters with positive sign to drive 
                    cosmic expansion, mimicking dark energy effects.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Eigen-Decomposition</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="A = PDP^{-1}, D = \text{diag}(-\lambda_1, -\lambda_2, -\lambda_3, -\lambda_4)" />
                  </div>
                  <p className="mt-2">
                    Negative eigenvalues <InlineMath math="\lambda_i < 0" /> drive repulsive behavior along 
                    corresponding eigendirections in spacetime.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Matrix Evolution</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\frac{dX}{dt} = [H, X]" />
                  </div>
                  <p className="mt-2">
                    The state matrix <InlineMath math="X" /> evolves according to a cosmic Hamiltonian 
                    <InlineMath math="H" />, similar to quantum mechanical evolution in the Heisenberg picture.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Temporal Geometry</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\Delta t \sim \frac{1}{\lambda_{\text{min}}(A)}" />
                  </div>
                  <p className="mt-2">
                    Time flow is linked to the minimum eigenvalue of the anti-gravity tensor, suggesting 
                    accelerated time flow during periods of increased curvature repulsion.
                  </p>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="comparison" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">AGDEF vs ΛCDM</CardTitle>
                <CardDescription className="text-white/70">
                  Key differences in predictions and implications
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-6">
                <div className="overflow-x-auto">
                  <table className="w-full border-collapse">
                    <thead>
                      <tr className="border-b border-dark-pink/20">
                        <th className="text-left p-4 font-medium text-dark-pink">Observable</th>
                        <th className="text-left p-4 font-medium text-dark-pink">ΛCDM Prediction</th>
                        <th className="text-left p-4 font-medium text-dark-pink">AGDEF Prediction</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-dark-pink/10">
                      <tr>
                        <td className="p-4">
                          <strong>Supernova d<InlineMath math="L" />(z)</strong>
                          <p className="text-sm text-white/60 mt-1">
                            Luminosity distance vs redshift
                          </p>
                        </td>
                        <td className="p-4">
                          Constant <InlineMath math="\Lambda" />
                          <p className="text-sm text-white/60 mt-1">
                            Static dark energy density
                          </p>
                        </td>
                        <td className="p-4">
                          Slight evolution in curvature trace
                          <p className="text-sm text-white/60 mt-1">
                            Time-varying anti-gravity contribution
                          </p>
                        </td>
                      </tr>
                      <tr>
                        <td className="p-4">
                          <strong>ISW Effect</strong>
                          <p className="text-sm text-white/60 mt-1">
                            Integrated Sachs-Wolfe in voids
                          </p>
                        </td>
                        <td className="p-4">
                          Weak signal in voids
                          <p className="text-sm text-white/60 mt-1">
                            Uniform dark energy effect
                          </p>
                        </td>
                        <td className="p-4">
                          Stronger void signal due to local anti-gravity
                          <p className="text-sm text-white/60 mt-1">
                            Enhanced by spatial curvature variations
                          </p>
                        </td>
                      </tr>
                      <tr>
                        <td className="p-4">
                          <strong>CMB Sachs-Wolfe</strong>
                          <p className="text-sm text-white/60 mt-1">
                            Temperature fluctuations
                          </p>
                        </td>
                        <td className="p-4">
                          Static curvature
                          <p className="text-sm text-white/60 mt-1">
                            Fixed gravitational potential
                          </p>
                        </td>
                        <td className="p-4">
                          Dynamic curvature changes heat photons
                          <p className="text-sm text-white/60 mt-1">
                            Time-evolving anti-gravity field
                          </p>
                        </td>
                      </tr>
                      <tr>
                        <td className="p-4">
                          <strong>BAO Peak Shift</strong>
                          <p className="text-sm text-white/60 mt-1">
                            Baryon acoustic oscillations
                          </p>
                        </td>
                        <td className="p-4">
                          Fixed ruler
                          <p className="text-sm text-white/60 mt-1">
                            Constant comoving scale
                          </p>
                        </td>
                        <td className="p-4">
                          Slight redshift-dependent shift
                          <p className="text-sm text-white/60 mt-1">
                            Modified by evolving curvature
                          </p>
                        </td>
                      </tr>
                      <tr>
                        <td className="p-4">
                          <strong>Void Dynamics</strong>
                          <p className="text-sm text-white/60 mt-1">
                            Evolution of cosmic voids
                          </p>
                        </td>
                        <td className="p-4">
                          Slow evolution
                          <p className="text-sm text-white/60 mt-1">
                            Standard gravitational collapse
                          </p>
                        </td>
                        <td className="p-4">
                          Rapid expansion due to repulsion
                          <p className="text-sm text-white/60 mt-1">
                            Enhanced by anti-gravity field
                          </p>
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>

                <div className="mt-8">
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Key Distinguishing Features</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div className="bg-zinc-900/50 p-4 rounded-md">
                      <h4 className="font-medium text-dark-pink mb-2">ΛCDM Model</h4>
                      <ul className="list-disc pl-6 space-y-2 text-sm">
                        <li>Constant dark energy density</li>
                        <li>Uniform expansion rate</li>
                        <li>Standard gravitational dynamics</li>
                        <li>Fixed comoving scales</li>
                        <li>Weak void evolution</li>
                      </ul>
                    </div>
                    <div className="bg-zinc-900/50 p-4 rounded-md">
                      <h4 className="font-medium text-dark-pink mb-2">AGDEF Theory</h4>
                      <ul className="list-disc pl-6 space-y-2 text-sm">
                        <li>Time-varying anti-gravity field</li>
                        <li>Spatially dependent expansion</li>
                        <li>Modified gravitational dynamics</li>
                        <li>Evolving comoving scales</li>
                        <li>Enhanced void expansion</li>
                      </ul>
                    </div>
                  </div>
                </div>

                <div className="mt-8">
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Observational Tests</h3>
                  <p className="mb-4">
                    The key differences between AGDEF and ΛCDM can be tested through:
                  </p>
                  <ul className="list-disc pl-6 space-y-2">
                    <li>
                      <strong>High-redshift supernovae</strong> to detect dark energy evolution
                    </li>
                    <li>
                      <strong>Void-ISW cross-correlations</strong> to measure enhanced void signals
                    </li>
                    <li>
                      <strong>BAO measurements at z &gt; 2</strong> to detect scale evolution
                    </li>
                    <li>
                      <strong>Void growth rate</strong> to test enhanced expansion
                    </li>
                  </ul>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="implications" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Theoretical Implications</CardTitle>
                <CardDescription className="text-white/70">
                  Consequences and potential applications of AGDEF theory
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-6">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Cosmic Structure Formation</h3>
                  <div className="space-y-4">
                    <p>
                      The AGDEF theory provides natural explanations for several observed cosmic phenomena:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>Cosmic Expansion:</strong> Repulsive eigenmodes from higher dimensions drive 
                        Hubble acceleration, explaining the observed expansion rate.
                      </li>
                      <li>
                        <strong>Void Structures:</strong> Negative field lines may repel galaxies, naturally 
                        forming the observed cosmic void structures.
                      </li>
                      <li>
                        <strong>Dark Matter Effects:</strong> Gravitational lensing could result from curvature 
                        flows rather than exotic particles, eliminating the need for dark matter particles.
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Theoretical Stability</h3>
                  <div className="space-y-4">
                    <p>
                      The AGDEF framework exhibits several fundamental properties:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>Non-locality:</strong> Dark energy is inherently non-local and cannot be 
                        quantized in the standard sense, aligning with holographic principles.
                      </li>
                      <li>
                        <strong>Indirect Observability:</strong> The field is only observable through its 
                        geometric warping effects, not through direct measurement.
                      </li>
                      <li>
                        <strong>Information Encoding:</strong> The theory suggests the possibility of encoding 
                        information into curvature signatures, potentially enabling cosmic-scale encryption schemes.
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Potential Applications</h3>
                  <div className="space-y-4">
                    <p>
                      The theory opens up several intriguing possibilities:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>FTL Travel:</strong> Local manipulation of the anti-gravity matrix could 
                        potentially create warp drive-like phenomena, enabling faster-than-light travel.
                      </li>
                      <li>
                        <strong>Cosmic Engineering:</strong> Understanding and controlling the anti-gravity 
                        field could lead to new methods of manipulating spacetime geometry.
                      </li>
                      <li>
                        <strong>Quantum Gravity:</strong> The matrix-based approach provides a potential 
                        bridge between quantum mechanics and gravity through the evolution equation 
                        <InlineMath math="\frac{dX}{dt} = [H, X]" />.
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Future Directions</h3>
                  <p>
                    The AGDEF theory suggests several promising avenues for future research:
                  </p>
                  <ul className="list-disc pl-6 space-y-2 mt-2">
                    <li>
                      Development of experimental methods to detect and measure curvature signatures
                    </li>
                    <li>
                      Investigation of potential quantum information encoding in spacetime geometry
                    </li>
                    <li>
                      Exploration of practical applications in propulsion and energy systems
                    </li>
                    <li>
                      Study of the relationship between AGDEF and other quantum gravity approaches
                    </li>
                  </ul>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="simulation" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">AGDEF Simulation Results</CardTitle>
                <CardDescription className="text-white/70">
                  Haskell-based cosmological model predictions
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-8">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Haskell Implementation</h3>
                  <div className="space-y-4">
                    <p>
                      The AGDEF theory is implemented in Haskell using linear algebra operations, modeling the 
                      5D to 4D projection of the anti-gravity tensor:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto">
                      <pre className="text-sm font-mono">
{`-- Tensor definition (simplified 4D projection of 5D matrix)
stressTensor5D :: Matrix Double
stressTensor5D = (5><5)
  [ 1, 0.2, 0.1, 0.3, 0.0
  , 0.2, 1.4, 0.2, 0.1, 0.0
  , 0.1, 0.2, 1.6, 0.4, 0.0
  , 0.3, 0.1, 0.4, 2.0, 0.0
  , 0.0, 0.0, 0.0, 0.0, 1.0 ]

-- Anti-gravity tensor from stress tensor
antiGravityTensor5D :: Double -> Matrix Double -> Matrix Double
antiGravityTensor5D k t = scale (-k) t

-- 5D to 4D projection matrix (drops last dimension)
projectionMatrix :: Matrix Double
projectionMatrix = (5><4)
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 1, 0
  , 0, 0, 0, 1
  , 0, 0, 0, 0 ]  -- drops 5th dim

-- Perform dimensional projection
projectAntiGravity :: Matrix Double -> Matrix Double
projectAntiGravity ag5 = tr projectionMatrix <> ag5 <> projectionMatrix

main :: IO ()
main = do
  let k = 1.0
      ag5 = antiGravityTensor5D k stressTensor5D
      ag4 = projectAntiGravity ag5
      (eigvals, _) = eig ag4
  putStrLn "Projected 4D Anti-Gravity Tensor Eigenvalues:"
  print $ toList eigvals`}
                      </pre>
                    </div>
                    <p className="mt-4">
                      This implementation models:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        5D stress-energy tensor with non-zero components in the fifth dimension
                      </li>
                      <li>
                        Anti-gravity tensor as negative scaling of the stress tensor
                      </li>
                      <li>
                        Explicit projection from 5D to 4D spacetime
                      </li>
                      <li>
                        Eigenvalue analysis of the projected tensor to determine repulsive behavior
                      </li>
                    </ul>
                    <p className="mt-4">
                      The projection operation <InlineMath math="A_{\mu\nu} = P^\top A_{MN} P" /> is implemented 
                      using matrix multiplication, where <InlineMath math="P" /> is the projection matrix that 
                      drops the fifth dimension. The resulting 4D tensor's eigenvalues determine the strength 
                      and direction of the anti-gravity effect.
                    </p>
                    <div className="mt-4 bg-zinc-900/50 p-4 rounded-md">
                      <h4 className="text-dark-pink mb-2">Output Analysis</h4>
                      <p className="text-sm">
                        The program computes and displays the eigenvalues of the projected 4D anti-gravity tensor. 
                        Negative eigenvalues indicate repulsive behavior along corresponding eigendirections, 
                        which manifests as dark energy in our observable universe.
                      </p>
                    </div>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Model Components</h3>
                  <div className="space-y-4">
                    <p>
                      Our AGDEF simulation computes key cosmological observables:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="H(z)^2 = \rho_m + \rho_{\text{AGDEF}}(z)" />
                    </div>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="d_L(z) = (1+z)\int_0^z \frac{dz'}{H(z')}" />
                    </div>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\mu(z) = 5\log_{10}(d_L(z)/10\text{ pc})" />
                    </div>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">AGDEF Energy Evolution</h3>
                  <p className="mb-4">
                    The anti-gravity tensor trace evolves with redshift:
                  </p>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\text{Tr}(A_{\mu\nu})(z) = 0.7 + 0.05\exp(-z/1.0)" />
                  </div>
                  <p className="mt-4">
                    This time-varying component drives the accelerated expansion, unlike ΛCDM's constant dark energy.
                  </p>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Simulation Results</h3>
                  <p className="mb-4">
                    Comparison of AGDEF predictions with observational data:
                  </p>
                  <SimulationPlots />
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Statistical Analysis</h3>
                  <div className="space-y-4">
                    <p>
                      We evaluate the model fit using chi-squared analysis:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\chi^2 = \sum_i \frac{(\mu_{\text{AGDEF}}(z_i) - \mu_{\text{obs}}(z_i))^2}{\sigma_i^2}" />
                    </div>
                    <p>
                      This allows direct comparison with ΛCDM predictions and observational uncertainties.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="observations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Observational Evidence</CardTitle>
                <CardDescription className="text-white/70">
                  Comparison with Pantheon+ supernovae and Planck CMB data
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-8">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Pantheon+ Supernovae</h3>
                  <div className="space-y-4">
                    <p>
                      Analysis of 1700+ Type Ia supernovae from redshifts <InlineMath math="z = 0.01" /> to 
                      <InlineMath math="z = 2.3" />:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        Distance modulus measurements with uncertainties
                      </li>
                      <li>
                        AGDEF predictions show slight evolution in dark energy density
                      </li>
                      <li>
                        Comparison with ΛCDM at high redshifts
                      </li>
                    </ul>
                    <ObservationPlots />
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Planck CMB Anisotropies</h3>
                  <div className="space-y-4">
                    <p>
                      Analysis of the Integrated Sachs-Wolfe (ISW) effect:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\dot{\Phi}(z) \propto \frac{d}{dz}\left(\frac{1}{H(z)a^2}\right)" />
                    </div>
                    <p>
                      AGDEF predicts enhanced ISW signal at large angles (<InlineMath math="\ell < 30" />) due to:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        Time-varying curvature potential
                      </li>
                      <li>
                        Enhanced void expansion
                      </li>
                      <li>
                        Non-uniform dark energy distribution
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Baryon Acoustic Oscillations</h3>
                  <div className="space-y-4">
                    <p>
                      Angular diameter distance measurements:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="D_A(z) = \frac{d_L(z)}{(1+z)^2}" />
                    </div>
                    <p>
                      AGDEF predictions for BAO peak positions show:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        Slight redshift-dependent shifts
                      </li>
                      <li>
                        Modified sound horizon scale
                      </li>
                      <li>
                        Testable with future high-redshift surveys
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Future Tests</h3>
                  <div className="space-y-4">
                    <p>
                      Upcoming observations will provide crucial tests:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>LSST/JWST:</strong> High-redshift supernovae to test dark energy evolution
                      </li>
                      <li>
                        <strong>CMB-S4:</strong> Enhanced ISW measurements in cosmic voids
                      </li>
                      <li>
                        <strong>DESI/Euclid:</strong> Precise BAO measurements at high redshifts
                      </li>
                      <li>
                        <strong>Gravitational Waves:</strong> Potential signatures in the stochastic background
                      </li>
                    </ul>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="aging" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">AGDEF Theory & Aging</CardTitle>
                <CardDescription className="text-white/70">
                  A curvature-based model of biological time
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-8">
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">The Cosmological-Time Connection</h3>
                  <p className="mb-4">
                    In standard physics, proper time experienced by an observer is tied to the local spacetime 
                    curvature via the metric tensor:
                  </p>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="d\tau^2 = g_{\mu\nu}dx^\mu dx^\nu" />
                  </div>
                  <p className="mt-4">
                    In AGDEF theory, regions of net repulsive curvature are governed by the trace of the 
                    projected anti-gravity tensor:
                  </p>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\text{Tr}(A_{\mu\nu}) > 0 \Rightarrow \text{faster proper time}" />
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Time as a Function of Energy Tension</h3>
                  <p className="mb-4">
                    We hypothesize biological aging rate <InlineMath math="\alpha" /> is a function of local 
                    anti-gravity curvature tension:
                  </p>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="\alpha(x) = \frac{d\tau_{\text{bio}}}{dt} \sim f(\text{Tr}(A_{\mu\nu}(x)))" />
                  </div>
                  <div className="mt-4 space-y-2">
                    <p>Where:</p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <InlineMath math="\alpha(x) > 1" />: accelerated biological time (aging faster)
                      </li>
                      <li>
                        <InlineMath math="\alpha(x) < 1" />: decelerated aging (aging slower)
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Biological Entropy in a Curved Field</h3>
                  <p className="mb-4">
                    Aging is modeled as entropy accumulation in the curved field:
                  </p>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="S(t) = \int_0^t \alpha(x(t')) \cdot \sigma(t') dt'" />
                  </div>
                  <div className="mt-4 space-y-2">
                    <p>Where:</p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <InlineMath math="\sigma(t)" />: intrinsic entropy production rate
                      </li>
                      <li>
                        <InlineMath math="\alpha(x)" />: environmental modifier from AGDEF curvature
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Predictions and Explanations</h3>
                  <div className="overflow-x-auto">
                    <table className="w-full border-collapse">
                      <thead>
                        <tr className="border-b border-dark-pink/20">
                          <th className="text-left p-4 font-medium text-dark-pink">Aging Phenomenon</th>
                          <th className="text-left p-4 font-medium text-dark-pink">AGDEF Interpretation</th>
                        </tr>
                      </thead>
                      <tbody className="divide-y divide-dark-pink/10">
                        <tr>
                          <td className="p-4">
                            <strong>Astronauts aging slower in orbit</strong>
                          </td>
                          <td className="p-4">
                            Reduced curvature from Earth mass = slower <InlineMath math="\alpha" /> from gravity
                          </td>
                        </tr>
                        <tr>
                          <td className="p-4">
                            <strong>Longevity in low-stress environments</strong>
                          </td>
                          <td className="p-4">
                            Minimal local mass-energy fluctuations = smoother AGDEF field = slower entropy
                          </td>
                        </tr>
                        <tr>
                          <td className="p-4">
                            <strong>Meditation slowing time perception</strong>
                          </td>
                          <td className="p-4">
                            Lowers neural energy fluctuations = reduces local field excitations
                          </td>
                        </tr>
                        <tr>
                          <td className="p-4">
                            <strong>Cancer accelerating aging</strong>
                          </td>
                          <td className="p-4">
                            Local chaotic mass-energy distributions = higher <InlineMath math="\text{Tr}(A_{\mu\nu})" />
                          </td>
                        </tr>
                        <tr>
                          <td className="p-4">
                            <strong>DNA methylation clocks</strong>
                          </td>
                          <td className="p-4">
                            Reflection of cumulative entropy <InlineMath math="S(t)" /> over dynamic AGDEF-curved time
                          </td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Metaphysical Implications</h3>
                  <div className="space-y-4">
                    <p>
                      AGDEF theory suggests several profound implications for our understanding of life and aging:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        The universe as a biological field, with aging as a curvature-aware entropy flow
                      </li>
                      <li>
                        Lifeforms as curvature-dependent entropic systems, evolving on a dynamic tensor field
                      </li>
                      <li>
                        Immortality requiring flattening of local AGDEF curvature field to zero
                      </li>
                    </ul>
                  </div>
                </div>

                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-4">Future Research Directions</h3>
                  <div className="space-y-4">
                    <p>
                      The AGDEF-aging connection opens several promising research avenues:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        Development of anti-aging technologies that measure and neutralize AGDEF curvature disturbances
                      </li>
                      <li>
                        Study of environmental factors (EMF, urban stress) on local tensor fields and aging
                      </li>
                      <li>
                        Investigation of exotic propulsion fields and their effects on biological time
                      </li>
                      <li>
                        Exploration of meditation and consciousness effects on local spacetime curvature
                      </li>
                    </ul>
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