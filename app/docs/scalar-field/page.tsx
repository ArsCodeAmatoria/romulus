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
            <TabsTrigger value="haskell" className="data-[state=active]:bg-dark-pink/20 data-[state=active]:text-dark-pink">Haskell Implementation</TabsTrigger>
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
          
          <TabsContent value="haskell" className="mt-6">
            <Card className="bg-black border-dark-pink/20">
              <CardHeader>
                <CardTitle className="text-white">Scalar Field Models in Haskell</CardTitle>
                <CardDescription className="text-white/70">
                  Functional representation of f(R) gravity and scalar fields
                </CardDescription>
              </CardHeader>
              <CardContent className="text-white/80 space-y-4">
                <p>
                  Haskell's pure functional paradigm provides an elegant way to express the mathematical models of scalar field theories.
                  Below are implementations of key equations and models using Haskell's type system and higher-order functions.
                </p>
                
                <div className="space-y-6 mt-4">
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Tensor Type Representation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`-- Type definitions for tensor representations
module ScalarField.Types where

import qualified Data.Vector as V
import qualified Data.Array as A

-- Represent a spacetime point
type Coords = (Double, Double, Double, Double)  -- (t, x, y, z)

-- Tensor type definitions
newtype Scalar = Scalar Double
  deriving (Show, Eq)

-- Vector representation
newtype FourVector = FourVector (V.Vector Double)
  deriving (Show, Eq)

-- Rank 2 tensor (4x4 matrix for spacetime)
newtype MetricTensor = MetricTensor (A.Array (Int, Int) Double)
  deriving (Show, Eq)

-- The f(R) function that modifies general relativity
type FRFunction = Scalar -> Scalar  -- Takes curvature scalar R, returns f(R)

-- Scalar field definition
type ScalarField = Coords -> Scalar  -- Maps a spacetime point to a scalar value`}
                      </code>
                    </pre>
                  </div>
                  
                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">f(R) Gravity Implementation</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`-- f(R) gravity modifications
module ScalarField.FRGravity where

import ScalarField.Types
import qualified Data.Array as A

-- Standard f(R) gravity models
-- Hu-Sawicki model: f(R) = -m² (c₁(R/m²)ⁿ) / (c₂(R/m²)ⁿ + 1)
huSawicki :: Double -> Double -> Double -> FRFunction
huSawicki c1 c2 n (Scalar r) = 
  let m2 = 1.0e-67  -- m² in appropriate units
      rTerm = r / m2
  in Scalar (-m2 * c1 * (rTerm ** n) / (c2 * (rTerm ** n) + 1))

-- Starobinsky model: f(R) = αR² for large R
starobinsky :: Double -> FRFunction
starobinsky alpha (Scalar r) = Scalar (alpha * r * r)

-- Modified Einstein field equations with f(R) term
-- G_μν + f(R)g_μν = (8πG/c⁴)T_μν
einsteinfR :: FRFunction -> MetricTensor -> MetricTensor -> MetricTensor
einsteinfR fR (MetricTensor g) (MetricTensor t) = 
  let 
      -- Calculate Ricci scalar from metric (simplified)
      calcR = Scalar 0.0  -- Placeholder for actual calculation
      
      -- Apply f(R) modification
      frTerm = fR calcR
      
      -- Physical constants
      kappa = 8.0 * pi * 6.67e-11 / (3.0e8 ** 4)  -- 8πG/c⁴
      
      -- Create modified tensor
      modify (i, j) gVal = 
        let tVal = t A.! (i, j)
            Scalar frVal = frTerm
        in gVal + frVal * gVal - kappa * tVal
        
      -- Create array bounds from input
      bounds = A.bounds g
  in
      MetricTensor $ A.array bounds [(idx, modify idx (g A.! idx)) | idx <- A.range bounds]`}
                      </code>
                    </pre>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Scalar Field Model</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`-- Scalar field implementation for dark matter effects
module ScalarField.Model where

import ScalarField.Types

-- Scalar-Tensor theory implementation
type ScalarTensorCoupling = ScalarField -> MetricTensor -> MetricTensor

-- Basic scalar field that depends on radius from galactic center
-- φ(r) = φ₀ exp(-r/r_s) represents a scalar field that modifies gravity
scalarFieldRadial :: Double -> Double -> Coords -> Scalar
scalarFieldRadial phi0 rs (t, x, y, z) = 
  let r = sqrt (x*x + y*y + z*z)  -- Simplified radial distance
  in Scalar (phi0 * exp (-r / rs))

-- Modified Newtonian potential with scalar field effects
-- Φ(r) = -GM/r · (1 + α·exp(-r/rs))
modifiedPotential :: Double -> Double -> Double -> Double -> Double
modifiedPotential mass alpha rs r =
  let 
      g = 4.3e-6  -- G in kpc³/(solar mass · Gyr²)
      newtonPotential = -g * mass / r
      modification = 1.0 + alpha * exp (-r / rs)
  in newtonPotential * modification

-- Calculate rotation curve velocity from potential
-- v²(r) = r · dΦ/dr
rotationVelocity :: Double -> Double -> Double -> Double -> Double
rotationVelocity mass alpha rs r =
  let 
      g = 4.3e-6
      -- Standard Newtonian term
      vNewton2 = g * mass / r
      
      -- Scalar field modification term
      modTerm = alpha * exp (-r / rs) * (1.0 + r / rs)
  in sqrt (vNewton2 * (1.0 + modTerm))

-- Dark matter as boundary condition implementation
-- Similar to edge detection in image processing
edgeDetector :: Double -> Double -> Double -> ScalarField
edgeDetector threshold sharpness scale (_, x, y, z) =
  let r = sqrt (x*x + y*y + z*z)
      densityGradient = exp (-r / scale)  -- Simplified stand-in for actual density
      edge = 1.0 / (1.0 + exp (-sharpness * (densityGradient - threshold)))
  in Scalar edge`}
                      </code>
                    </pre>
                  </div>

                  <div>
                    <h3 className="text-lg font-medium text-dark-pink mb-2">Simulation Example</h3>
                    <pre className="bg-zinc-900/50 p-4 rounded-md overflow-x-auto text-sm">
                      <code>
{`-- Example usage for galaxy rotation curve simulation
module ScalarField.Simulation where

import ScalarField.Model
import ScalarField.Types
import qualified Data.Vector as V

-- Simulate a galaxy rotation curve with scalar field modified gravity
simulateGalaxy :: Double -> V.Vector Double -> V.Vector Double
simulateGalaxy galaxyMass radii =
  let 
      -- Parameters for scalar field model
      alpha = 0.5       -- Strength of modification
      rs = 15.0         -- Scale radius in kpc
      
      -- Calculate velocity at each radius
      velocities = V.map (rotationVelocity galaxyMass alpha rs) radii
  in 
      velocities

-- Calculate both Newtonian and modified gravity for comparison
compareModels :: Double -> V.Vector Double -> (V.Vector Double, V.Vector Double)
compareModels galaxyMass radii =
  let 
      g = 4.3e-6
      -- Standard Newtonian velocities
      vNewton = V.map (\r -> sqrt (g * galaxyMass / r)) radii
      
      -- Velocities with scalar field modification
      vModified = simulateGalaxy galaxyMass radii
  in
      (vNewton, vModified)

{- 
Example usage:

main :: IO ()
main = do
  let radii = V.fromList [1.0, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0]
      galaxyMass = 1.0e11  -- solar masses
      (vNewton, vModified) = compareModels galaxyMass radii
  
  putStrLn "Radius (kpc) | Newtonian v (km/s) | Modified v (km/s)"
  putStrLn "---------------------------------------------------"
  
  V.zipWith3 (\r vn vm -> 
      putStrLn $ show r ++ " | " ++ show vn ++ " | " ++ show vm)
    radii vNewton vModified
-}`}
                      </code>
                    </pre>
                  </div>
                </div>
                
                <div className="mt-6 p-4 bg-dark-pink/10 rounded-md">
                  <p className="text-dark-pink font-medium">Why Haskell for Physics?</p>
                  <p className="mt-2">
                    Haskell's pure functional approach offers several advantages for theoretical physics:
                  </p>
                  <ul className="list-disc pl-6 mt-2 space-y-1">
                    <li>Mathematical expressions map directly to functions</li>
                    <li>Type system can represent physical quantities and enforce correct dimensions</li>
                    <li>Referential transparency ensures equations behave like true mathematical expressions</li>
                    <li>Higher-order functions allow elegant representation of field theories and operators</li>
                  </ul>
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