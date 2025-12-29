package phi

/**
 * Val: Runtime values in Phi
 * A value is a constructor applied to arguments: VCon(name, args)
 */
enum Val:
  case VCon(name: String, args: List[Val])

object Val:
  import Val.*
  
  extension (v: Val)
    def show: String = v match
      case VCon(name, Nil) => name
      case VCon(name, args) => s"$name(${args.map(_.show).mkString(", ")})"
