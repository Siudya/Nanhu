package xsphase
import firrtl.AnnotationSeq
import firrtl.annotations._
import firrtl.ir._
import firrtl.options.Phase
import firrtl.renamemap.MutableRenameMap
import firrtl.stage.FirrtlCircuitAnnotation

object PrefixHelper {
  val coreNamePat = "XSTile_?[0-9]+"
  var prefix = "bosc_"
  def StatementsWalker(stmt:Statement):Statement = {
    stmt match {
      case s: DefInstance =>{
        if(s.module.matches(coreNamePat)){
          println(s"Rename ${s.module} calling to XSTile!")
          s.copy(module = prefix + "XSTile")
        } else {
          s.copy(module = prefix + s.module)
        }
      }
      case s: Conditionally => s.copy(conseq = StatementsWalker(s.conseq), alt = StatementsWalker(s.alt))
      case s: Block => {
        val stmts = s.stmts.map(StatementsWalker)
        s.copy(stmts = stmts)
      }
      case other => other
    }
  }
}

class Prefix extends Phase {
  override def prerequisites: Seq[Nothing] = Seq.empty
  override def optionalPrerequisites: Seq[Nothing] = Seq.empty
  override def optionalPrerequisiteOf: Seq[Nothing] = Seq.empty
  override def invalidates(a: Phase) = false
  private val prefix = PrefixHelper.prefix
  private val renameMap = MutableRenameMap()
  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val prefixedAS = annotations.flatMap {
      case a: FirrtlCircuitAnnotation =>
        val mods = a.circuit.modules.map {
          case mm@Module(_, name, _, body) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            val nst = PrefixHelper.StatementsWalker(body)
            mm.copy(name = prefix + name, body = nst)
          }
          case em@ExtModule(_, name, _, defname, _) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            em.copy(name = prefix + name, defname = prefix + defname)
          }
          case im@IntModule(_, name, _, _, _) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            im.copy(name = prefix + name)
          }
          case other => other
        }
        val nc = a.circuit.copy(modules = mods, main = prefix + a.circuit.main)
        Some(FirrtlCircuitAnnotation(nc))
      case a => Some(a)
    }
    val redirectedAS = prefixedAS.flatMap{
      a => a.update(renameMap)
    }
    redirectedAS
  }
}
