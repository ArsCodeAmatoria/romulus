'use client';

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { DisplayMath, InlineMath } from "@/components/ui/math";
import SpacetimeCurvature from "@/components/interactive/SpacetimeCurvature";

export default function GeneralRelativityPage() {
  return (
    <div className="min-h-screen pt-24 pb-16 px-4">
      <div className="container mx-auto max-w-4xl">
        <h1 className="text-4xl font-bold mb-6 text-white neon-glow">General Relativity & Tensor Modifications</h1>
        <p className="text-white/80 text-lg mb-8">
          Einstein's General Relativity provides the foundation for modern understanding of gravity. 
          Modified gravity approaches often involve adjustments to the Einstein field equations through tensor modifications.
        </p>
        
        <Tabs defaultValue="overview" className="mb-10">
          <TabsList className="bg-black border border-dark-pink/20">
            <TabsTrigger value="overview" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Overview</TabsTrigger>
            <TabsTrigger value="equations" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Key Equations</TabsTrigger>
            <TabsTrigger value="modifications" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Tensor Modifications</TabsTrigger>
            <TabsTrigger value="simulation" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Simulation</TabsTrigger>
          </TabsList>
          
          <TabsContent value="overview" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">General Relativity</CardTitle>
                <CardDescription className="text-white/70">
                  Einstein's theory of how matter and energy curve spacetime
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  General Relativity (GR) revolutionized our understanding of gravity. Rather than 
                  treating gravity as a force, Einstein's theory describes it as the curvature of 
                  spacetime caused by mass and energy.
                </p>
                <p>
                  The core principle is that massive objects create a curvature in the fabric of 
                  spacetime, and this curvature guides the motion of other objects. This is captured 
                  mathematically by the Einstein field equations:
                </p>
                <div className="my-4 bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                  <DisplayMath math="G_{\mu\nu} + \Lambda g_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                </div>
                <p>
                  Where <InlineMath math="G_{\mu\nu}" /> is the Einstein tensor, <InlineMath math="\Lambda" /> is the cosmological 
                  constant, <InlineMath math="g_{\mu\nu}" /> is the metric tensor, <InlineMath math="G" /> is Newton's gravitational 
                  constant, <InlineMath math="c" /> is the speed of light, and <InlineMath math="T_{\mu\nu}" /> is the 
                  stress-energy tensor.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="equations" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Key Equations</CardTitle>
                <CardDescription className="text-white/70">
                  The mathematical foundation of General Relativity
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <div className="space-y-6">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Einstein Field Equations</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="R_{\mu\nu} - \frac{1}{2}R g_{\mu\nu} + \Lambda g_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                    </div>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Einstein Tensor</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="G_{\mu\nu} = R_{\mu\nu} - \frac{1}{2}R g_{\mu\nu}" />
                    </div>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Ricci Tensor</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="R_{\mu\nu} = \partial_{\lambda}\Gamma^{\lambda}_{\mu\nu} - \partial_{\nu}\Gamma^{\lambda}_{\mu\lambda} + \Gamma^{\lambda}_{\mu\nu}\Gamma^{\sigma}_{\lambda\sigma} - \Gamma^{\sigma}_{\mu\lambda}\Gamma^{\lambda}_{\nu\sigma}" />
                    </div>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Geodesic Equation</h3>
                    <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                      <DisplayMath math="\frac{d^2x^{\mu}}{d\tau^2} + \Gamma^{\mu}_{\alpha\beta}\frac{dx^{\alpha}}{d\tau}\frac{dx^{\beta}}{d\tau} = 0" />
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="modifications" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Tensor Modifications</CardTitle>
                <CardDescription className="text-white/70">
                  Extending General Relativity to account for dark matter effects
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  To explain dark matter effects without introducing new particles, 
                  researchers have proposed various modifications to the Einstein field equations.
                  These modifications typically involve adding new terms to the action or directly
                  to the field equations.
                </p>
                
                <div>
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Modified Einstein Equations</h3>
                  <div className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-center">
                    <DisplayMath math="G_{\mu\nu} + \Lambda g_{\mu\nu} + E_{\mu\nu} = \frac{8\pi G}{c^4}T_{\mu\nu}" />
                  </div>
                  <p className="mt-2">
                    Where <InlineMath math="E_{\mu\nu}" /> represents additional tensor terms that modify gravity at galactic scales.
                  </p>
                </div>
                
                <div className="mt-6">
                  <h3 className="text-lg font-medium text-dark-pink mb-2">Example: Haskell Implementation</h3>
                  <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                    <code>
{"module ModifiedGravity where\n\nimport Data.List (zipWith)\n\n-- | Gravitational constant (m³/kg/s²)\ng :: Double\ng = 6.67430e-11\n\n-- | Calculate acceleration under a simple modified gravity model\n-- that transitions between Newtonian gravity and modified regime\nmodifiedGravityAcceleration :: Double -> Double -> Double -> Double\nmodifiedGravityAcceleration r mass a0 = a_mod\n  where\n    -- Newtonian acceleration\n    a_newton = g * mass / r^2\n    \n    -- Modification factor (example of a simple interpolation function)\n    mu = a_newton / (a_newton + a0)\n    \n    -- Modified acceleration\n    a_mod = a_newton / mu\n\n-- | Convenience function for array calculations\nmap2 :: (a -> b -> c) -> [a] -> [b] -> [c]\nmap2 = zipWith\n\n-- | Example: Calculate galaxy rotation curve\ncalculateGalaxyRotationCurve :: IO ()\ncalculateGalaxyRotationCurve = do\n  -- Constants\n  let a0 = 1.2e-10  -- Acceleration scale (m/s²) where modification becomes significant\n      m_galaxy = 1e11 * 1.989e30  -- Galaxy mass (kg)\n      kpc_to_meters = 3.086e19  -- Conversion from kpc to meters\n      \n  -- Generate radii from 1 to 50 kpc (converted to meters)\n  let radii = map (* kpc_to_meters) [1.0, 1.5..50.0]\n  \n  -- Calculate accelerations\n  let a_newton = map (\\r -> g * m_galaxy / r^2) radii\n      a_modified = map (\\r -> modifiedGravityAcceleration r m_galaxy a0) radii\n  \n  -- Convert to rotation velocities (v = sqrt(r*a))\n  let v_newton = map2 (\\r a -> sqrt (r * a)) radii a_newton\n      v_modified = map2 (\\r a -> sqrt (r * a)) radii a_modified\n  \n  -- The modified model produces a flatter rotation curve at large radii\n  -- similar to observed galaxy rotation curves\n  putStrLn $ \"Radius (kpc) | Newtonian v (km/s) | Modified v (km/s)\"\n  putStrLn $ replicate 50 '-'\n  \n  -- Print a few sample points (converting back to kpc and km/s)\n  let sampleIndices = [0, 9, 19, 29, 39, 49]\n      samples = [(i, r / kpc_to_meters, v_n / 1000, v_m / 1000) | \n                 (i, r, v_n, v_m) <- zip4 [0..] radii v_newton v_modified, \n                 i `elem` sampleIndices]\n  \n  mapM_ (\\(_, r, v_n, v_m) -> \n      putStrLn $ show (round r :: Int) ++ \"            | \" ++ \n                 show (round v_n :: Int) ++ \"               | \" ++ \n                 show (round v_m :: Int)) samples\n\n-- Benefits of using Haskell for theoretical physics:\n-- * Pure functions map closely to mathematical equations\n-- * Strong type system prevents dimensional errors\n-- * Lazy evaluation allows for efficient handling of infinite data structures\n-- * Elegant expression of complex mathematical concepts\n-- * Pattern matching simplifies implementation of case-specific physics equations"}
                    </code>
                  </pre>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
          
          <TabsContent value="simulation" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Simulation</CardTitle>
                <CardDescription className="text-white/70">
                  Interactive demonstration of spacetime curvature
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80">
                <p className="mb-4">
                  This interactive visualization demonstrates how mass curves spacetime in both 
                  standard General Relativity and in modified gravity theories. 
                  The grid represents spacetime being deformed by the central mass.
                </p>
                <div className="bg-zinc-900/50 p-4 rounded-md">
                  <SpacetimeCurvature />
                </div>
                <p className="mt-4">
                  The simulation above demonstrates how mass curves spacetime, and how this 
                  curvature affects the motion of objects. In modified gravity theories, this 
                  curvature is enhanced at certain scales without additional dark matter.
                </p>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
} 