# values in double braces will be replaced by the template engine. Do not modify
# "orbs" is the basename (without suffix) for cube files containing orbitals
# "cdddens" is the basename (without suffix) for cube files containing CDDs, electron cubes and hole cubes
# "viewpoint" is obtained from a VMD state file and contains settings for perspective and camera
# Be aware of the "quit" at the end. VMD wont exit otherwise and would have to be closed manually

# isovalue for orbitals and densities
set IsoOrb  0.02
set IsoDens 0.002

# tweak some display settings
# also possible via vmdrc
axes location off

# start plotting loops
set Iter 0

# loop over all orbitals
foreach Orb [list {{ orbs }}] {
  # load cube into VMD
  mol load cube ${Orb}.cube

  # set camera perspective
  {{ viewpoint }}

  # delete the default representation after molecule has been loaded (start clean)
  mol delrep 0 $Iter
  mol selection all

  # draw ball and stick model of molecular structure
  mol material AOChalky
  mol representation CPK 1.0 0.3 50.0 50.0
  mol addrep $Iter

  # draw positive and negative isosurface for the orbitals
  mol material Glass2
  mol color ColorID 0
  mol representation Isosurface $IsoOrb  0 0 0 1 1
  mol addrep $Iter

  mol material Glass2
  mol color ColorID 3
  mol representation Isosurface [expr -1 * $IsoOrb] 0 0 0 1 1
  mol addrep $Iter

  # write tachyon scene file without rendering it
  render Tachyon ${Orb}.dat

  # delete all 3 representations, that have been created for the orbitals
  mol delrep 0 $Iter
  mol delrep 0 $Iter
  mol delrep 0 $Iter

  mol delete $Iter
  incr Iter
}

# loop over all densities
foreach Dens [list {{ cdddens }}] {
  # load cube into VMD
  mol load cube ${Dens}.cube

  # set camera perspective
  {{ viewpoint }}

  # delete the default representation after molecule has been loaded (start clean)
  mol delrep 0 $Iter
  mol selection all

  # draw ball and stick model of molecular structure
  mol material AOChalky
  mol representation CPK 1.0 0.3 50.0 50.0
  mol addrep $Iter

  # draw positive and negative isosurface for the orbitals
  mol material Glass2
  # check if this is a hole and change color for holes to the one of the negative isosurface
  if {[string match */hole* ${Dens}]} {
      mol color ColorID 3
  } else {
      mol color ColorID 0
  }
  mol representation Isosurface $IsoDens  0 0 0 1 1
  mol addrep $Iter

  mol material Glass2
  mol color ColorID 3
  mol representation Isosurface [expr -1 * $IsoDens] 0 0 0 1 1
  mol addrep $Iter

  # write tachyon scene file without rendering it
  render Tachyon ${Dens}.dat

  # delete all 3 representations, that have been created for the orbitals
  mol delrep 0 $Iter
  mol delrep 0 $Iter
  mol delrep 0 $Iter

  mol delete $Iter
  incr Iter
}

quit
