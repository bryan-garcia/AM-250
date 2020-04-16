FUNCTION f(x)
    IMPLICIT NONE
    REAL :: x, f
    f = SIN( x ) + EXP( COS( x * SIN( X ) ) )
END FUNCTION f
