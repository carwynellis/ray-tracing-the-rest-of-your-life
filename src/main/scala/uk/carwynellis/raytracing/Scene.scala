package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable._
import uk.carwynellis.raytracing.material.{Dielectric, DiffuseLight, Lambertian, Metal}
import uk.carwynellis.raytracing.texture.{CheckerBoard, ConstantTexture, ImageTexture, NoiseTexture}
import uk.carwynellis.raytracing.hitable.transform.FlipNormals.HitableToFlipNormalsOps
import uk.carwynellis.raytracing.hitable.transform.{RotateY, Translate}

case class Scene(
  private val objects: HitableList,
  raySources: HitableList,
  camera: Camera,
  time0: Double = 0.0,
  time1: Double = 0.0,
) {

  def withAspectRatio(a: Double) = this.copy(camera = camera.copy(aspectRatio = a))

  def withStartTime(t: Double) = this.copy(
    time0 = t,
    camera = camera.copy(time0 = t))

  def withEndTime(t: Double) = this.copy(
    time1 = t,
    camera = camera.copy(time1 = t)
  )

  def world = BoundingVolumeHierarchy.fromHitables(objects.hitables, time0, time1)

}

object Scene {

  val staticScene = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5, Lambertian(ConstantTexture(Vec3(0.5, 0.5, 0.6)))),
    Sphere(Vec3(0, -100.5, -1), 100, Lambertian(ConstantTexture(Vec3(0.8, 0.8, 0.0)))),
    Sphere(Vec3(1, 0, -1), 0.5, Metal(ConstantTexture(Vec3(0.8, 0.6, 0.2)), 0.3)),
    Sphere(Vec3(-1, 0, -1), -0.5, Dielectric(1.5))
  ))

  val twoPerlinSpheres = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(NoiseTexture(20))),
  ))

  private val earthTexturePath = "/Users/carwyn/Downloads/earthmap.jpg"

  val earthTexture: ImageTexture = ImageTexture.fromPath(earthTexturePath).fold(
    ex => throw ex, // Not much we can do except throw the exception if we failed to create the texture
    identity
  )

  val perlinAndImageSpheres = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(earthTexture))
  ))

  val perlinAndLight = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, DiffuseLight(ConstantTexture(Vec3(4, 4, 4))))
  ))

  val simpleLightScene = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(4))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(NoiseTexture(4))),
    Sphere(Vec3(0, 7, 0), 2, DiffuseLight(ConstantTexture(Vec3(1, 1, 1)))),
    XYRectangle(3, 5, 1, 3, -2, DiffuseLight(ConstantTexture(Vec3(1, 1, 1))))
  ))

  val cornellBoxScene = {
    val red = Lambertian(ConstantTexture(Vec3(0.65, 0.05, 0.05)))
    val white = Lambertian(ConstantTexture(Vec3(0.73, 0.73, 0.73)))
    val green = Lambertian(ConstantTexture(Vec3(0.12, 0.45, 0.15)))
    val light = DiffuseLight(ConstantTexture(Vec3(15, 15, 15)))

    val lightSource = XZRectangle(213, 343, 227, 332, 554, light)
    val glassSphere = Sphere(Vec3(190, 90, 190), 90, Dielectric(1.5))

    val objects = List(
      lightSource,
      YZRectangle(0, 555, 0, 555, 555, green).flipNormals,
      YZRectangle(0, 555, 0, 555, 0, red),
      XZRectangle(0, 555, 0, 555, 555, white).flipNormals,
      XZRectangle(0, 555, 0, 555, 0, white),
      XYRectangle(0, 555, 0, 555, 555, white).flipNormals,
      glassSphere,
      Translate(RotateY(Box(Vec3(0, 0, 0), Vec3(165, 330, 165), Dielectric(1.5)), 15), Vec3(265, 0, 295)),
    )

    Scene(
      objects = HitableList(objects),
      raySources = HitableList(List(
        lightSource,
        glassSphere,
      )),
      camera = Camera(
        origin = Vec3(278, 278, -800),
        target = Vec3(278, 278, 0),
        upVector = Vec3(0, 1, 0),
        verticalFieldOfView = 40,
        aspectRatio = 1,
        aperture = 0.0,
        focusDistance = 10,
      )
    )
  }

  val cornellSmoke: HitableList = {
    val red = Lambertian(ConstantTexture(Vec3(0.65, 0.05, 0.05)))
    val white = Lambertian(ConstantTexture(Vec3(0.73, 0.73, 0.73)))
    val green = Lambertian(ConstantTexture(Vec3(0.12, 0.45, 0.15)))
    val light = DiffuseLight(ConstantTexture(Vec3(7, 7, 7)))

    val box1 = Translate(RotateY(Box(Vec3(0, 0, 0), Vec3(165, 165, 165), white), -18), Vec3(130, 0, 65))
    val box2 = Translate(RotateY(Box(Vec3(0, 0, 0), Vec3(165, 330, 165), white), 15), Vec3(265, 0, 295))

    HitableList(List(
      YZRectangle(0, 555, 0, 555, 555, green).flipNormals,
      YZRectangle(0, 555, 0, 555, 0, red),
      XZRectangle(213, 343, 227, 332, 554, light),
      XZRectangle(0, 555, 0, 555, 555, white).flipNormals,
      XZRectangle(0, 555, 0, 555, 0, white),
      XYRectangle(0, 555, 0, 555, 555, white).flipNormals,
      ConstantMedium(box1, 0.01, ConstantTexture(Vec3(1, 1, 1))),
      ConstantMedium(box2, 0.01, ConstantTexture(Vec3(0, 0, 0)))
    ))
  }

  val finalScene: HitableList = {
    val boxCount = 20
    val sphereCount = 1000

    val white = Lambertian(ConstantTexture(Vec3(0.73, 0.73, 0.73)))
    val ground = Lambertian(ConstantTexture(Vec3(0.48, 0.83, 0.53)))
    val light = DiffuseLight(ConstantTexture(Vec3(7, 7, 7)))

    val boxList = (0 until boxCount).map { i =>
      (0 until boxCount).map { j =>
        val w = 100
        val x0 = -1000 + (i * w)
        val z0 = -1000 + (j * w)
        val y0 = 0
        val x1 = x0 + w
        val y1 = 100*(math.random() + 0.01)
        val z1 = z0 + w
        Box(Vec3(x0,y0,z0), Vec3(x1,y1,z1), ground);
      }
    }

    val spheres = (0 until sphereCount).map { i =>
      Sphere(Vec3(165*math.random(), 165*math.random(), 165*math.random()), 10, white);
    }

    val glassSurface = Sphere(Vec3(360, 150, 145), 70, Dielectric(1.5))
    val mist = Sphere(Vec3(0, 0, 0), 5000, Dielectric(1.5))

    val centre = Vec3(400, 400, 200)

    HitableList(List(
      BoundingVolumeHierarchy.fromHitables(boxList.flatten.toList, 0, 1),
      XZRectangle(123, 423, 147, 412, 554, light),
      MovingSphere(centre, centre+Vec3(30, 0, 0), 50, Lambertian(ConstantTexture(Vec3(0.7, 0.3, 0.1))), 0, 1),
      Sphere(Vec3(260, 150, 45), 50, Dielectric(1.5)),
      Sphere(Vec3(0, 150, 145), 50, Metal(ConstantTexture(Vec3(0.8, 0.8, 0.9)), 0.8)),
      glassSurface,
      ConstantMedium(glassSurface, 0.2, ConstantTexture(Vec3(0.2, 0.4, 0.9))),
      ConstantMedium(mist, 0.0001, ConstantTexture(Vec3(1, 1, 1))),
      Sphere(Vec3(400, 200, 400), 100, Lambertian(earthTexture)),
      Sphere(Vec3(220, 280, 300), 80, Lambertian(NoiseTexture(0.1))),
      Translate(RotateY(BoundingVolumeHierarchy.fromHitables(spheres.toList, 0, 1), 15), Vec3(-100, 270, 395))
    ))
  }

  def randomScene(): HitableList = {

    val lowerBound = -11
    val upperBound = 11

    val range = lowerBound until upperBound

    def generateSpheres: List[Hitable] = range.flatMap { a =>
      range.flatMap { b =>
        val materialSelector = math.random()

        val centre = Vec3(
          x = a + 0.9 * math.random(),
          y = 0.2,
          z = b + 0.9 * math.random()
        )
        generateSphere(centre, materialSelector)
      }
    }.toList

    def generateSphere(c: Vec3, m: Double): Option[Hitable] = if ((c - Vec3(4, 0.2, 0)).length > 0.9) {
      if (m < 0.8)
        Some(MovingSphere(
          centre0 = c,
          centre1 = c + Vec3(0, math.random() * 0.5, 0),
          radius = 0.2,
          time0 = 0.0,
          time1 = 1.0,
          material = Lambertian(ConstantTexture(Vec3(math.random(), math.random(), math.random())))
        ))
      else if (m < 0.95) {
        def randomColor = 0.5 * (1 + math.random())
        Some(Sphere(c, 0.2,
          Metal(
            ConstantTexture(Vec3(randomColor, randomColor, randomColor)),
            0.5 * math.random()
          )
        ))
      }
      else Some(Sphere(c, 0.2, Dielectric(1.5)))
    }
    else None

    val checkerboard = CheckerBoard(
      odd = ConstantTexture(Vec3(0.1, 0.1, 0.3)),
      even = ConstantTexture(Vec3(0.9, 0.9, 0.9))
    )

    val scene = List(
      Sphere(Vec3(0, -1000, 0), 1000, Lambertian(checkerboard)),
      Sphere(Vec3(0, 1, 0), 1, Dielectric(1.5)),
      Sphere(Vec3(-4, 1, 0), 1, Lambertian(ConstantTexture(Vec3(0.4, 0.2, 0.1)))),
      Sphere(Vec3(4, 1, 0), 1, Metal(ConstantTexture(Vec3(0.7, 0.6, 0.5)), 0))
    ) ++ generateSpheres
    HitableList(scene)
  }

}
