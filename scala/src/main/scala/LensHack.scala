object LensHack extends App {

   case class Address(number: Int, street: String)

   case class Person(name: String, age: Int, address: Address)

   implicit class LensStateOpts[S, A](l: Lens[S, A]) {
      def liftState: State[S, A] = State(s => (l.get(s), s))

      def :=(a: A): State[S, A] = State[S, A](s => (a, l.set(s, a)))

      def %= [B](f: A => A): State[S, A] = for {
         x <- liftState
         y <- this := f(x)
      } yield y

   }

   case class Lens[A, B](
      get: A => B,
      set: (A, B) => A
   ) {
      def modify[C](a: A, f: B => B): A = {
         set(a, get(a))
      }

      def andThen[C](r: Lens[B, C]): Lens[A, C] = {
         val composedGet: A => C = get andThen r.get

         val composedSet: (A, C) => A = (a: A, c: C) => {
            val b: B = get(a)
            val newB: B = r.set(b, c)
            set(a, newB)
         }

         Lens(
            get = composedGet,
            set = composedSet
         )
      }
   }

   case class State[S, A](run: S => (A, S)) {
      def map[B](f: A => B): State[S, B] = State { s =>
         val (a, s2) = run(s)
         (f(a), s2)
      }

      def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
         val (a, s2) = run(s)
         f(a).run(s2)
      }

      def widen[Big](l: Lens[Big, S]): State[Big, A] = State { big =>
         val (a, s) = run(l.get(big))
         (a, l.set(big, s))
      }
   }

   object State {
      def get[S]: State[S, S] = State(s => (s, s))

      def put[S](s: S): State[S, Unit] = State(_ => ((), s))

      def unit[S, A](a: A): State[S, A] = State(s => (a, s))
   }

   def product[A1, A2, B1, B2](l: Lens[A1, A2], r: Lens[B1, B2]): Lens[(A1, B1), (A2, B2)] = {
      val composedGet: (A1, B1) => (A2, B2) = (a1: A1, b1: B1) => (l.get(a1), r.get(b1))
      val composedSet: ((A1, B1), (A2, B2)) => (A1, B1) = {
         case ((a1, b1), (a2, b2)) => (l.set(a1, a2), r.set(b1, b2))
      }

      Lens(
         composedGet.tupled,
         composedSet
      )
   }

   def choice[A1, A2, B1, B2](l: Lens[A1, A2], r: Lens[B1, B2]): Lens[Either[A1, B1], Either[A2, B2]] = {
      val composedGet: Either[A1, B1] => Either[A2, B2] = {
         case Left(a1) => Left[A2, B2](l.get(a1))
         case Right(b1) => Right[A2, B2](r.get(b1))
      }

      val composedSet: (Either[A1, B1], Either[A2, B2]) => Either[A1, B1] = {
         case (Left(a1), Left(a2)) => Left(l.set(a1, a2))
         case (Left(a1), _) => Left(a1)
         case (Right(b1), Right(b2)) => Right(r.set(b1, b2))
         case (Right(b1), _) => Right(b1)
      }

      Lens(
         composedGet,
         composedSet
      )
   }

   def inc[S](l: Lens[S, Int]): State[S, Int] =
      for {
         x <- l.liftState
         _ <- l %= (_ + 1)
      } yield x

   // https://github.com/tonymorris/lets-lens/blob/master/src/Lets/Lens.hs
   //   product ::
   //       Lens s t a b
   //   -> Lens q r c d
   //       -> Lens (s, q) (t, r) (a, c) (b, d)
   //   product _ _ =

   val _address = Lens[Person, Address](
      get = _.address,
      set = (p: Person, a: Address) => p.copy(address = a)
   )

   val _age = Lens[Person, Int](
      get = _.age,
      set = (p: Person, a: Int) => p.copy(age = a)
   )

   val _street = Lens[Address, String](
      get = _.street,
      set = (a: Address, s: String) => a.copy(street = s)
   )

   val _personStreet: Lens[Person, String] = _address andThen _street

   val testValue = Person(
      name = "Eli",
      age = 31,
      address = Address(
         number = 123,
         street = "Fred St"
      )
   )

   val addressAction: State[Address, Address] = for {
      address <- State.get[Address]
      _ <- State.put(address.copy(number = 3))
   } yield address

   val personAction = for {
      _ <- addressAction.widen(_address)
   } yield ()

   println(personAction.run(testValue))

//
//   val personAction: State[Person, Person] = for {
//      p <- State.get[Person].fo
//   } yield address
//
//   println(action.run(testValue.address))

//   val action = inc(_age)
//
//   println(action.run(testValue))

//   println("Get: " + _personStreet.get(testValue))
//   println("Set: " + _personStreet.set(testValue, "Bob St"))
}
