      double precision function f(t, p, v)
	double precision v, t, p
	f=(3.0D0*p*(v**3.0D0))-((8.0D0*t+p)*(v**2))+(9.0D0*v-3.0D0)
      return
	end function

	double precision Function df(t, p, v)
	double precision v, t, p
	df= 9.0D0*p*(v**2.0D0) - (8.0D0*t+p)*(2.0D0*v) + 9.0D0
      return
	end function

************************************************************************


      double precision function meta(t,v)
      double precision t,v
      meta=4.0d0*t*v**3.0d0-9.0d0*v**2.0d0+6.0d0*v-1.0d0
      return
      end function


      double precision function dmeta(t,v)
      double precision t,v
      dmeta=12.0d0*t*v**2-18.0d0*v+6.0d0
      return
      end function

*************************************************************************

      double precision function estat(v,t)
      double precision v,t
      estat=(8.0d0*t/(3.0d0*v-1.0d0))-3.0d0/(v**2)
      return
      end function
