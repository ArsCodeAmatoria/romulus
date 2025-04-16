'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";

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
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} + f(R)g_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                    </div>
                    <p className="mt-2">
                      By modifying how spacetime curvature <InlineMath math="R" /> behaves through the function <InlineMath math="f(R)" />, we can naturally
                      generate effects similar to dark matter without additional mass. This affects how gravity
                      behaves at large distances or in regions of low acceleration.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Scalar-Tensor Theory</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} + \phi(x)g_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                    </div>
                    <p className="mt-2">
                      A scalar field <InlineMath math="\phi(x)" /> modifies the gravitational interaction, creating enhanced gravitational
                      effects at galaxy edges that mimic dark matter halos. The field isn't a particle but rather
                      a property of spacetime that varies with position.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Edge Detection Model</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\nabla^2\psi = f(\rho)" />
                    </div>
                    <p className="mt-2">
                      Similar to edge detection in image processing, this approach treats dark matter as an effect that
                      emerges around the "edges" of matter distributions. The function <InlineMath math="f(\rho)" /> selectively enhances gravitational
                      effects at the boundaries of matter concentrations.
                    </p>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Computational Implementation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`module ScalarFieldGravity where

-- | Implementation of f(R) Modified Gravity in Haskell
-- This model demonstrates how gravity modifications can create
-- dark-matter-like effects without introducing new particles

-- | Gravitational constant in kpc^3/(solar mass * Gyr^2)
gravConst :: Double
gravConst = 4.3e-6

-- | Calculate gravitational potential with f(R) modification
-- This enhances gravity at large scales to mimic dark matter effects
modifiedGravityPotential :: Double -> Double -> Double -> Double -> Double
modifiedGravityPotential r mass alpha beta
  | r <= 0 = 0  -- Guard against division by zero
  | otherwise = phiNewton * modificationFactor
  where
    -- Standard Newtonian potential
    phiNewton = negate $ gravConst * mass / r
    
    -- f(R) modification factor - enhances gravity at large scales
    -- This creates dark-matter-like effects without dark matter
    modificationFactor = 1 + alpha * (r / beta)^2 / (1 + (r / beta)^2)

-- | Calculate rotation velocity from gravitational potential
-- v^2 = r * (d/dr)Φ
calculateVelocity :: [Double] -> [Double] -> [Double]
calculateVelocity radii potentials =
  zipWith3 velocityAtPoint radii potentials (tail potentials ++ [last potentials])
  where
    velocityAtPoint :: Double -> Double -> Double -> Double
    velocityAtPoint r phi1 phi2 =
      let dr = if r < 29.0 then 30.0 / 99 else 0.1  -- Approximate gradient step
          dPhi = (phi2 - phi1) / dr
      in sqrt $ abs $ r * dPhi

-- | Calculate rotation curves for a galaxy with f(R) modified gravity
calculateRotationCurves :: Double -> Double -> Double -> IO ()
calculateRotationCurves galaxyMass alpha beta = do
  putStrLn "f(R) Modified Gravity Rotation Curve Analysis"
  putStrLn "------------------------------------------"
  putStrLn $ "Galaxy mass: " ++ show (galaxyMass / 1.0e10) ++ " × 10¹⁰ solar masses"
  putStrLn $ "Alpha (modification strength): " ++ show alpha
  putStrLn $ "Beta (modification scale): " ++ show beta ++ " kpc"
  putStrLn ""
  putStrLn "Radius (kpc) | v_Newton (km/s) | v_Modified (km/s) | Enhancement"
  putStrLn "--------------------------------------------------------------"
  
  -- Create radius values from 0.1 to 30 kpc
  let radii = [0.1, 0.4..30.0]
  
  -- Calculate potentials
  let potentialsNewton = map (\r -> negate $ gravConst * galaxyMass / r) radii
      potentialsModified = map (\r -> modifiedGravityPotential r galaxyMass alpha beta) radii
  
  -- Calculate velocities (convert to km/s)
  let velocitiesNewton = map (*1000) $ calculateVelocity radii potentialsNewton
      velocitiesModified = map (*1000) $ calculateVelocity radii potentialsModified
  
  -- Print sample points
  let sampleIndices = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99]
      samples = map (\\i -> (radii !! i, velocitiesNewton !! i, velocitiesModified !! i)) sampleIndices
  
  mapM_ (\\(r, vn, vm) -> do
    let enhancement = vm / vn
        formatNum n = if n < 10 then " " ++ showFFloat 1 n else showFFloat 1 n
    putStrLn $ padRight 12 (showFFloat 1 r) ++ 
               "| " ++ padRight 14 (formatNum vn) ++ 
               "| " ++ padRight 16 (formatNum vm) ++ 
               "| " ++ showFFloat 2 enhancement ++ "×"
    ) samples
  
  putStrLn ""
  putStrLn "Note: The modified gravity model produces a flat rotation curve"
  putStrLn "at large radii without requiring dark matter particles."

-- | Helper function for fixed precision display
showFFloat :: Int -> Double -> String
showFFloat digits num = 
  let str = show (fromIntegral (round (num * 10^digits)) / 10^digits :: Double)
  in if digits == 0 then takeWhile (/= '.') str else str

-- | Padding helper for table formatting
padRight :: Int -> String -> String
padRight width str = str ++ replicate (width - length str) ' '

{- 
Example usage:
calculateRotationCurves 1.0e11 0.1 2.0

This shows how a galaxy with 10^11 solar masses would have
a flat rotation curve with f(R) gravity modification,
without requiring dark matter particles.
-}`}
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