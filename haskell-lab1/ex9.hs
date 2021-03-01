roots :: (Double,Double,Double)->(Double,Double)
roots (a,b,c) = ((-b-d)/e, (-b+d)/e)
    where d=sqrt(b*b-4*a*c)
          e=2*a

{-
To
też
jest
komentarz
-}

unitVec2D :: (Double,Double)->(Double,Double)
unitVec2D (a,b) = (a/lenght,b/lenght)
    where lenght=sqrt(a^2+b^2)

unitVec3D :: (Double,Double,Double)->(Double,Double,Double)
unitVec3D (a,b,c)=(a/lenght,b/lenght,c/lenght)
    where lenght=sqrt(a^2+b^2+c^2)

triangle :: (Double,Double,Double)->Double
triangle (a,b,c)=sqrt(p*(p-a)*(p-b)*(p-c))
    where p=(a+b+c)/2

