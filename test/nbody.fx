import Math

val SolarMass = 4.0 * Math.Pi * Math.Pi
val Year = 365.24

type Vec = {x: double; y: double; z: double}
operator + (a: Vec, b: Vec) = Vec {x=a.x+b.x, y=a.y+b.y, z=a.z+b.z}
operator - (a: Vec, b: Vec) = Vec {x=a.x-b.x, y=a.y-b.y, z=a.z-b.z}
operator * (a: Vec, s: double) = Vec {x=a.x*s, y=a.y*s, z=a.z*s}
fun dot(a: Vec, b: Vec) = a.x*b.x + a.y*b.y + a.z*b.z
fun distance2(a: Vec) = dot(a, a)

type Object =
{
    pos: Vec;
    vel: Vec;
    mass: double
}

val sun = Object {pos=Vec {x=0.0, y=0.0, z=0.0}, vel=Vec {x=0.0, y=0.0, z=0.0}, mass=SolarMass}
val jupiter = Object {
    pos=Vec {
        x=4.84143144246472090e+00,
        y=-1.16032004402742839e+00,
        z=-1.03622044471123109e-01},
    vel=Vec {
        x=1.66007664274403694e-03 * Year,
        y=7.69901118419740425e-03 * Year,
        z=-6.90460016972063023e-05 * Year},
    mass=9.54791938424326609e-04 * SolarMass
}
val saturn = Object {
    pos=Vec {
        x=8.34336671824457987e+00,
        y=4.12479856412430479e+00,
        z=-4.03523417114321381e-01},
    vel=Vec {
        x=-2.76742510726862411e-03 * Year,
        y=4.99852801234917238e-03 * Year,
        z=2.30417297573763929e-05 * Year},
    mass=2.85885980666130812e-04 * SolarMass
}
val uranus = Object {
    pos=Vec {
        x=1.28943695621391310e+01,
        y=-1.51111514016986312e+01,
        z=-2.23307578892655734e-01},
    vel=Vec {
        x=2.96460137564761618e-03 * Year,
        y=2.37847173959480950e-03 * Year,
        z=-2.96589568540237556e-05 * Year},
    mass=4.36624404335156298e-05 * SolarMass
}
val neptune = Object {
    pos=Vec {
        x=1.53796971148509165e+01,
        y=-2.59193146099879641e+01,
        z=1.79258772950371181e-01},
    vel=Vec {
        x=2.68067772490389322e-03 * Year,
        y=1.62824170038242295e-03 * Year,
        z=-9.51592254519715870e-05 * Year},
    mass=5.15138902046611451e-05 * SolarMass
}
val bodies = [sun, jupiter, saturn, uranus, neptune]

fun offsetMomentum()
{
    bodies[0] \=
    {
        vel = fold(p = Vec{x=0., y=0., z=0.}; body in bodies)
                p += body.vel * (body.mass / SolarMass)
    }
}

fun energy()
{
    val n = size(bodies)
    fold (e = 0.; i in 0:n)
    {
        val bi = bodies[i]
        e += 0.5 * bi.mass * dot(bi.vel, bi.vel) -
        (fold(e1 = 0.; j in (i+1):n)
        {
            val bj = bodies[j]
            val diff = bi.pos - bj.pos
            val distance = Math.sqrt(distance2(diff))
            e1 += (bi.mass * bj.mass) / distance
        })
    }
}

fun advance(dt: double)
{
    val n = size(bodies)
    for (i in 0:n)
    {
        var bi = bodies[i]
        for (j in (i+1):n)
        {
            val bj = bodies[j]
            val diff = bi.pos - bj.pos
            val dist2 = distance2(diff)
            val mag = dt / (dist2 * Math.sqrt(dist2))
            bi.vel -= diff * (bj.mass * mag)
            bodies[j].vel = bj.vel + diff * (bi.mass * mag)
        }
        bodies[i] = bi \ { pos=bi.pos + bi.vel * dt }
    }
}

offsetMomentum()
println(energy())
for (i in 0:50000000) advance(0.01)
println(energy())
println("Done")
