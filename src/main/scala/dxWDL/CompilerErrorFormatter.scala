package dxWDL

import scala.collection.JavaConverters._
import wdl.draft3.parser.WdlParser._
import wom.core._

case class CompilerErrorFormatter(resource: String,
                                  terminalMap: Map[Terminal, WorkflowSource]) {
    private def findFirstTerminal(astNode: AstNode) : Option[Terminal] = {
        Option(astNode) flatMap {
            case l: AstList =>
                val v: Vector[AstNode] = l.asScala.toVector
                v.flatMap(x => findFirstTerminal(x)).headOption
            case a: Ast => a.getAttributes.asScala.toMap.flatMap(
                { case (_, v) => findFirstTerminal(v) }
            ).headOption
            case t: Terminal => Option(t)
        }
    }

    private def line(t:Terminal): String = {
        terminalMap.get(t) match {
            case None => throw new Exception(s"Could not find terminal ${t} in source file ${resource}")
            case Some(x) => x.split("\n")(t.getLine - 1)
        }
    }

    private def pointToSource(t: Terminal): String = {
        s"${line(t)}\n${" " * (t.getColumn - 1)}^"
    }

    private def textualSource(t: Terminal) : String = {
        val lineNum = t.getLine
        s"${resource}, line ${lineNum}"
    }

    private def makeErrorMessage(astNode: AstNode, msg: String) : String = {
        val t: Terminal = findFirstTerminal(astNode).get
        s"""|${msg}
            |
            |${textualSource(t)}
            |${pointToSource(t)}
            |""".stripMargin
    }

    def compilerInternalError(astNode: AstNode, featureName: String) : String = {
        makeErrorMessage(astNode,
                         s"Should not reach this point in the code: ${featureName}")
    }

    def couldNotEvaluateType(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Could not evaluate the WDL type for expression")
    }

    def illegalCallName(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Illegal call name")
    }

    def illegalVariableName(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Illegal variable name")
    }

    def missingCallArgument(astNode: AstNode, msg:String) : String = {
        makeErrorMessage(astNode,
                         s"""|Call is missing a compulsory argument.
                             |${msg}""".stripMargin)
    }

    def missingVarRef(t: Terminal) : String = {
        makeErrorMessage(t, "Reference to missing variable")
    }

    def missingVarRef(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Reference to missing variable")
    }

    def notCurrentlySupported(astNode: AstNode, featureName: String) : String = {
        makeErrorMessage(astNode,
                         s"Not currently supported: ${featureName}")
    }

    def onlyFilesCanBeStreamed(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Only files can be streamed")
    }

    def taskInputDefaultMustBeConst(astNode: AstNode) : String = {
        makeErrorMessage(astNode, "Task input expression must be const or variable")
    }

    def taskNativeRuntimeBlockShouldBeEmpty(astNode: AstNode) : String = {
        makeErrorMessage(astNode,
                         s"""|This is a native task, a wrapper for a dnanexus call. It should
                             |have an empty runtime section.""".stripMargin)
    }

    def workflowInputDefaultMustBeConst(astNode: AstNode) : String = {
        makeErrorMessage(astNode,
                         "Workflow input expression must be const or variable")
    }
}
