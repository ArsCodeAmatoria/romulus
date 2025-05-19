'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";

export default function MONDPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">MOND and TeVeS</h1>
        <p className="text-white/80 text-lg mb-8">
          Modified Newtonian Dynamics (MOND) and its relativistic extension, Tensor-Vector-Scalar gravity (TeVeS),
          propose modifications to gravitational laws at low accelerations as alternatives to dark matter.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Key Equations</TabsTrigger>
            <TabsTrigger value="teves" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">TeVeS Theory</TabsTrigger>
            <TabsTrigger value="evidence" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Evidence & Challenges</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Modified Newtonian Dynamics</CardTitle>
                <CardDescription className="text-white/70">
                  A modification of gravity at low accelerations
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  In 1983, Israeli physicist Mordehai Milgrom proposed Modified Newtonian Dynamics (MOND) to explain galaxy 
                  rotation curves without invoking dark matter. MOND suggests that Newton's laws of gravity need modification 
                  at extremely low accelerations - typically below <InlineMath math="a_0 \approx 1.2 \times 10^{-10} \text{ m/s}^2" />, 
                  which is about one ten-billionth of Earth's surface gravity.
                </p>
                <p>
                  At these low accelerations, which are common in the outer regions of galaxies, MOND proposes that the gravitational 
                  force becomes proportional to the square of the distance rather than following the inverse-square law. This simple 
                  modification explains flat rotation curves naturally, without requiring additional unseen matter.
                </p>
                <div className="my-6 bg-zinc-900/50 p-4 rounded-md">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Key Insights</h3>
                  <ul className="list-disc pl-6 space-y-2">
                    <li>Proposes a fundamental acceleration scale <InlineMath math="a_0" /> in physics</li>
                    <li>Predicts flat rotation curves without dark matter</li>
                    <li>Naturally explains the Tully-Fisher relation between galaxy luminosity and rotation speed</li>
                    <li>Has been extended to relativistic frameworks like TeVeS</li>
                  </ul>
                </div>
                <p>
                  Rather than viewing dark matter as a substance, MOND reimagines it as a modification to our understanding of 
                  how gravity behaves at certain scales. This approach follows the historical pattern where apparent anomalies 
                  (like Mercury's orbit) led to deeper understanding of physical laws (General Relativity) rather than new substances.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Mathematical Framework</CardTitle>
                <CardDescription className="text-white/70">
                  The equations governing MOND
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">The MOND Equation</h3>
                    <p>
                      MOND modifies Newton's second law by introducing an interpolation function that depends on the acceleration:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="\mu\left(\frac{a}{a_0}\right)\vec{a} = \vec{a}_N" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="\vec{a}" /> is the actual acceleration, <InlineMath math="\vec{a}_N" /> is the 
                      Newtonian acceleration from visible matter, <InlineMath math="a_0" /> is the critical acceleration scale 
                      (approximately <InlineMath math="1.2 \times 10^{-10} \text{ m/s}^2" />), and <InlineMath math="\mu(x)" /> 
                      is an interpolation function with the properties:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="\mu(x) \approx \begin{cases} 
                      x & \text{for } x \ll 1 \\
                      1 & \text{for } x \gg 1
                      \end{cases}" />
                    </div>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Common Interpolation Functions</h3>
                    <p>
                      Various interpolation functions have been proposed, including:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="\mu(x) = \frac{x}{1+x} \quad \text{(simple)}" />
                      <div className="mt-2"></div>
                      <DisplayMath math="\mu(x) = \frac{x}{\sqrt{1+x^2}} \quad \text{(standard)}" />
                    </div>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Modified Poisson Equation</h3>
                    <p>
                      An alternative formulation modifies the Poisson equation for gravity:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="\nabla \cdot \left[ \mu\left(\frac{|\nabla\Phi|}{a_0}\right) \nabla\Phi \right] = 4\pi G \rho" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="\Phi" /> is the gravitational potential and <InlineMath math="\rho" /> is the 
                      mass density of visible matter.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Deep-MOND Regime</h3>
                    <p>
                      In the limit of very low accelerations (<InlineMath math="a \ll a_0" />), known as the deep-MOND regime:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="a = \sqrt{a_0 a_N}" />
                    </div>
                    <p className="mt-2">
                      This leads to flat rotation curves in galaxies, as the velocity becomes independent of radius:
                    </p>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center mt-2">
                      <DisplayMath math="v^4 = GMa_0" />
                    </div>
                    <p className="mt-2">
                      This predicts the Tully-Fisher relation, connecting galaxy luminosity (proportional to mass) 
                      to the fourth power of rotation velocity.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">MOND Implementation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{"module MOND where\n\n-- | Implementation of Modified Newtonian Dynamics (MOND) in Haskell\n-- Shows how functional programming naturally expresses physical laws\n\n-- | Gravitational constant (m³/kg/s²)\ngravConst :: Double\ngravConst = 6.67430e-11\n\n-- | MOND critical acceleration parameter (m/s²)\na0 :: Double\na0 = 1.2e-10  -- Approximately 1/10 billionth of Earth's surface gravity\n\n-- | Standard MOND interpolation function\n-- Used to transition between Newtonian and modified regimes\nmuFunction :: Double -> Double\nmuFunction x = x / sqrt (1 + x*x)\n\n-- | Simple interpolation function (alternative)\nmuSimple :: Double -> Double\nmuSimple x = x / (1 + x)\n\n-- | Calculate acceleration under MOND for a given radius and mass\n-- This implements the full MOND equation with proper handling of\n-- both deep-MOND and Newtonian regimes\nmondAcceleration :: Double -> Double -> Double -> Double\nmondAcceleration r mass a0' \n  | r <= 0 || mass <= 0 = 0  -- Guard against invalid inputs\n  | otherwise = case regime of\n      Newtonian  -> newtonianAcc\n      DeepMOND   -> sqrt (newtonianAcc * a0')\n      Transitional -> solveMondeqn newtonianAcc 10\n  where\n    -- Calculate Newtonian acceleration\n    newtonianAcc = gravConst * mass / (r * r)\n    \n    -- Determine which acceleration regime we're in\n    regime\n      | newtonianAcc > 100 * a0' = Newtonian\n      | newtonianAcc < 0.01 * a0' = DeepMOND\n      | otherwise = Transitional\n      \n    -- Iteratively solve MOND equation: a * mu(a/a0) = a_N\n    -- This converges quickly with a few iterations\n    solveMondeqn :: Double -> Int -> Double\n    solveMondeqn _ 0 = newtonianAcc  -- Fallback\n    solveMondeqn aN n = \n      let a' = aN / muFunction (a / a0')\n          a = solveMondeqn aN (n-1)  \n      in if abs (a' - a) < 1e-10 * a then a' else a'\n\n-- | Enumerate different acceleration regimes\ndata AccelerationRegime = Newtonian | DeepMOND | Transitional\n\n-- | Calculate rotation curves for a galaxy\ncalculateRotationCurve :: Double -> IO ()\ncalculateRotationCurve galaxyMass = do\n  putStrLn \"MOND Rotation Curve Analysis\"\n  putStrLn \"----------------------------\"\n  putStrLn $ \"Galaxy mass: \" ++ show (galaxyMass / (1.989e30)) ++ \" solar masses\"\n  putStrLn \"\"\n  putStrLn \"Radius (kpc) | v_Newton (km/s) | v_MOND (km/s) | Regime\"\n  putStrLn \"---------------------------------------------------------\"\n  \n  -- Sample points from 1 to 30 kpc\n  let kpcToM = 3.086e19\n      radii = [1, 5, 10, 15, 20, 25, 30]\n      radiusPoints = map (* kpcToM) radii\n  \n  -- Calculate and display results\n  mapM_ (\\(i, r) -> do\n    let aN = gravConst * galaxyMass / (r * r)\n        a = mondAcceleration r galaxyMass a0\n        vN = sqrt (r * aN) / 1000  -- km/s\n        vM = sqrt (r * a) / 1000   -- km/s\n        regime\n          | aN > 10 * a0 = \"Newtonian\"\n          | aN < 0.1 * a0 = \"Deep-MOND\"\n          | otherwise = \"Transition\"\n    \n    putStrLn $ padRight 12 (show i) ++ \n               \"| \" ++ padRight 14 (showF 1 vN) ++ \n               \"| \" ++ padRight 13 (showF 1 vM) ++ \n               \"| \" ++ regime\n    ) (zip radii radiusPoints)\n  \n  putStrLn \"\"\n  putStrLn \"Note: The MOND velocity curve becomes flat at large radii,\"\n  putStrLn \"while Newtonian velocity falls off as 1/sqrt(r)\"\n\n-- | Helper function for fixed precision display\nshowF :: Int -> Double -> String\nshowF digits num = \n  let str = show (fromIntegral (round (num * 10^digits)) / 10^digits :: Double)\n  in if digits == 0 then takeWhile (/= '.') str else str\n\n-- | Padding helper for table formatting\npadRight :: Int -> String -> String\npadRight width str = str ++ replicate (width - length str) ' '\n\n-- Example usage:\n-- calculateRotationCurve (5e10 * 1.989e30)  -- 50 billion solar masses"}
                      </code>
                    </pre>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="teves" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Tensor-Vector-Scalar Gravity</CardTitle>
                <CardDescription className="text-white/70">
                  The relativistic extension of MOND
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  While MOND successfully describes galactic dynamics, it lacks a relativistic framework needed for phenomena like 
                  gravitational lensing and cosmology. In 2004, Jacob Bekenstein proposed Tensor-Vector-Scalar gravity (TeVeS) as 
                  a relativistic generalization of MOND.
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">The Fields of TeVeS</h3>
                    <p>
                      TeVeS introduces three fields to generalize MOND:
                    </p>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>A <strong>tensor field</strong> (<InlineMath math="g_{\mu\nu}" />): Similar to the metric tensor in General Relativity</li>
                      <li>A <strong>vector field</strong> (<InlineMath math="U_\mu" />): A timelike vector field with unit norm</li>
                      <li>A <strong>scalar field</strong> (<InlineMath math="\phi" />): Creates the MOND-like behavior at low accelerations</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">The TeVeS Action</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="S = S_g + S_v + S_s + S_m" />
                    </div>
                    <p className="mt-2">
                      Where <InlineMath math="S_g" />, <InlineMath math="S_v" />, <InlineMath math="S_s" />, and <InlineMath math="S_m" /> 
                      are the actions for the tensor field, vector field, scalar field, and matter, respectively.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Key Features of TeVeS</h3>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>Reproduces MOND in the appropriate limit for galaxy dynamics</li>
                      <li>Provides proper gravitational lensing without dark matter</li>
                      <li>Preserves conservation laws and causality</li>
                      <li>Passes Solar System tests by reducing to General Relativity at high accelerations</li>
                      <li>Can be applied to cosmological scenarios</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Physical Interpretation</h3>
                    <p>
                      The additional fields in TeVeS can be interpreted as aspects of spacetime structure that become relevant 
                      at low accelerations. Rather than introducing dark matter particles, TeVeS suggests that the phenomena 
                      attributed to dark matter arise from a more complex structure of spacetime itself.
                    </p>
                    <p className="mt-2">
                      This connects to broader questions about whether spacetime is fundamental or emergent, and how it might 
                      behave differently in different acceleration regimes.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="evidence" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Evidence & Challenges</CardTitle>
                <CardDescription className="text-white/70">
                  Successes and limitations of MOND and TeVeS
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Observational Successes</h3>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>Galaxy Rotation Curves:</strong> MOND accurately predicts rotation curves for a wide variety of galaxies 
                        using only their visible matter distribution
                      </li>
                      <li>
                        <strong>Tully-Fisher Relation:</strong> MOND naturally explains the tight correlation between galaxy luminosity 
                        and rotation velocity
                      </li>
                      <li>
                        <strong>Dwarf Galaxies:</strong> Successfully predicts dynamics of dwarf galaxies, which are often problematic 
                        for dark matter models
                      </li>
                      <li>
                        <strong>External Field Effect:</strong> Unique prediction of MOND that galaxy dynamics are affected by external 
                        gravitational fields, which has been observed
                      </li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Major Challenges</h3>
                    <ul className="list-disc pl-6 space-y-2">
                      <li>
                        <strong>Galaxy Clusters:</strong> MOND does not fully account for the missing mass in galaxy clusters, still 
                        requiring some form of additional dark matter
                      </li>
                      <li>
                        <strong>Cosmic Microwave Background:</strong> The standard ΛCDM model provides a better fit to CMB observations
                      </li>
                      <li>
                        <strong>Large-Scale Structure:</strong> Dark matter models better explain the formation of large-scale structure 
                        in the universe
                      </li>
                      <li>
                        <strong>Bullet Cluster:</strong> The separation between gravitational lensing and visible matter in colliding 
                        clusters is often cited as evidence for particle dark matter
                      </li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Hybrid Approaches</h3>
                    <p>
                      Some researchers are exploring hybrid models that combine modified gravity with some form of dark matter, 
                      particularly to address galaxy cluster observations. These approaches suggest that even if MOND is correct 
                      at galactic scales, some form of dark matter might still be needed at larger scales.
                    </p>
                  </div>
                  
                  <div className="bg-zinc-900/50 p-4 rounded-md mt-6">
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Future Tests</h3>
                    <p>
                      Key tests that could distinguish between MOND and dark matter include:
                    </p>
                    <ul className="list-disc pl-6 mt-2 space-y-2">
                      <li>Precise measurements of the External Field Effect in more galaxies</li>
                      <li>Detailed studies of galaxy dynamics at the boundary of the MOND regime</li>
                      <li>Tests of TeVeS predictions for gravitational waves</li>
                      <li>Searching for signatures of modified gravity in cosmic structure</li>
                    </ul>
                    <p className="mt-2">
                      The ongoing debate between modified gravity and dark matter approaches continues to drive deeper investigations 
                      into the fundamental nature of gravity and spacetime.
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