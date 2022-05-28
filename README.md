CLT Creator

Commit 1:
- Initialized a custom 3D scene.
- Imported the CLT plank mesh structure and textures
- Basic camera orbit with drag and drop with mousedown events using subscriptions.
- Overall basic scene implemented.

Commit 2:
- Added Window resizing based on viewport upon initial load. 
- Changed Browser object type from Element -> Document.
- Added parameters to control overall scene width and height with % of pixels.

Commit 3: 
- Added legible comments throughout the code. 
- Added reference X, Y and Z axes - cylinders.
- Realistic CLT plank dimensions
- Fixed the resolution issue for the plank.

Commit 4:
- Added a Grid Texture.
- Created a Quad -> 64x64 (meters) grid on the XY plane.
- Arranged the XYZ axis cylinders into combined entity.
- Matched XYZ -> RGB for axis and made the mslightly larger. 

Commit 5:
- Changed units to centimeters for backward compatibility.
- Made UI fixes.
- All three axis rotations work now. 
- Rotations are in 90 degree increments.

Commit 6:
- Added Cutting mode to set to 2D automatically.
- Added Scrolling functionality in increments of 50 centimeters.
- Added Genration of Code string as a Model variable.
- Abstracted CltPlank type from Model.
- Added dynamic focus at midpoint of cltPlank. (supports rotations)
- Manually imported elm-3d-camp Wrapper3D module.

Commit 7:
- Two sawblades spawned in the scene. 
- Added animation for the sawblades.
- Added Frame from CLT plank
- Now the CLT plank rotates around its own axis rather than global axis.
- SawBlade corresponds to sliders for X and Y axis translations necessary for cutting.
- Guidelines added for sawblades. 

Commit 8:
- Refactoring of code and abstraction of CltPlank into another file.
- Cutting mechanism for the cltMain plank is done for 2 cuts simultaeneously. 
- The cut produces 4 smaller planks and makes the cltMain disappear. 
- The cut also correctly offsets the smaller planks to their original positions according to parent plank(cltmain).
- Centerpoint and Frame are also correctly allocated to the smaller planks.
- Need to implement the mechanism for just 1 cut. (Horizontally or Vertically)
- Need to implement selectability and focus for the smaller planks and overall individualization from CltList. (How?)
- Need more UI in cutter mode. (Using GraphicSVG elements)

Commit 9:
- Mechanism for Single axis cut is done.
- Overall UI has been improved.
- Individual plank selectability has been added.
- UI for cutting is more detailed.
- Abstraction of helper functions and main datastructure for CLTplank is done.
- Resized the CltPlank accurately w.r.t. real world dimensions.
- After cutting, individual planks can now be rotated. 
- Added Animation State with logical progession based on button input. 
- Added Camera animations. 
- Added Projection States with 3 different projections apt for CAD tools. 
- Perspective, Orthographic and Isometric projections have been added. 