// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja

import org.eclipse.lsp4j.debug.*
import org.eclipse.lsp4j.debug.launch.DSPLauncher
import org.eclipse.lsp4j.debug.services.{IDebugProtocolServer, IDebugProtocolClient}
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.net.{ServerSocket, Socket}
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Tracer for monitoring Pass execution.
  * 
  * The new Pass architecture has a single rewrite chokepoint, so we only need
  * 4 hooks (vs 10+ in the old Manip/Tracer system with branching/backtracking).
  */
trait PassTracer:
  /** Called before a pass starts executing on a node. */
  def beforePass(passName: String, root: Node): Unit
  
  /** Called after a pass completes execution. */
  def afterPass(passName: String, result: Node): Unit
  
  /** Called when a rewrite pattern matches and is about to execute.
    * This is at THE chokepoint where all rewrites happen.
    */
  def onRewriteMatch(ruleName: String, nodeSpan: NodeSpan): Unit
  
  /** Called after a rewrite completes. */
  def onRewriteComplete(ruleName: String, resultSpan: NodeSpan): Unit
end PassTracer

object PassTracer:
  /** No-op tracer that does nothing. Used as default. */
  object Nop extends PassTracer:
    def beforePass(passName: String, root: Node): Unit = ()
    def afterPass(passName: String, result: Node): Unit = ()
    def onRewriteMatch(ruleName: String, nodeSpan: NodeSpan): Unit = ()
    def onRewriteComplete(ruleName: String, resultSpan: NodeSpan): Unit = ()
  end Nop
  
  /** Simple logging tracer that prints to stdout. */
  class Logging extends PassTracer:
    def beforePass(passName: String, root: Node): Unit =
      val token = root.tokenOption.fold("<no token>")(_.toString)
      println(s"[PASS] Starting: $passName on $token")
    
    def afterPass(passName: String, result: Node): Unit =
      println(s"[PASS] Completed: $passName")
    
    def onRewriteMatch(ruleName: String, nodeSpan: NodeSpan): Unit =
      val nodeDesc = nodeSpan.headOption
        .flatMap(_.tokenOption)
        .fold("<empty>")(_.toString)
      println(s"[REWRITE] Match: $ruleName at $nodeDesc")
    
    def onRewriteComplete(ruleName: String, resultSpan: NodeSpan): Unit =
      val nodeDesc = resultSpan.headOption
        .flatMap(_.tokenOption)
        .fold("<empty>")(_.toString)
      println(s"[REWRITE] Complete: $ruleName → $nodeDesc")
  end Logging
end PassTracer

/** Minimal Debug Adapter using lsp4j for DAP protocol handling.
  * Just provides tracing output to the debug client.
  */
class DebugAdapter(port: Int) extends PassTracer, IDebugProtocolServer, AutoCloseable:
  private val serverSocket = ServerSocket(port)
  private var clientSocket: Socket | Null = null
  private var launcher: Launcher[IDebugProtocolClient] | Null = null
  private val traceEvents = mutable.ArrayBuffer[String]()
  
  // Start listening in background
  private val acceptThread = new java.lang.Thread(new Runnable {
    def run(): Unit =
      println(s"[DebugAdapter] Listening on port $port...")
      clientSocket = serverSocket.accept()
      println(s"[DebugAdapter] Client connected")
      
      val socket = clientSocket.nn
      val l = DSPLauncher.createServerLauncher(
        DebugAdapter.this,
        socket.getInputStream(),
        socket.getOutputStream()
      )
      launcher = l
      val listening = l.startListening()
      // Wait for it to complete
      try listening.get() catch case _: Exception => ()
  })
  acceptThread.setDaemon(true)
  acceptThread.start()
  
  // PassTracer implementation - send trace events to client
  def beforePass(passName: String, root: Node): Unit =
    sendOutput(s"Pass starting: $passName")
  
  def afterPass(passName: String, result: Node): Unit =
    sendOutput(s"Pass completed: $passName")
  
  def onRewriteMatch(ruleName: String, nodeSpan: NodeSpan): Unit =
    val nodeDesc = nodeSpan.headOption
      .flatMap(_.tokenOption)
      .fold("<empty>")(_.toString)
    sendOutput(s"  Rewrite: $ruleName at $nodeDesc")
  
  def onRewriteComplete(ruleName: String, resultSpan: NodeSpan): Unit =
    val nodeDesc = resultSpan.headOption
      .flatMap(_.tokenOption)
      .fold("<empty>")(_.toString)
    sendOutput(s"  Rewrite complete: $ruleName → $nodeDesc")
  
  private def sendOutput(message: String): Unit =
    traceEvents += message
    val l = launcher
    if l != null then
      val event = OutputEventArguments()
      event.setCategory("stdout")
      event.setOutput(message + "\n")
      l.getRemoteProxy().output(event)
  
  // Minimal IDebugProtocolServer implementation
  override def initialize(args: InitializeRequestArguments): CompletableFuture[Capabilities] =
    val caps = Capabilities()
    caps.setSupportsConfigurationDoneRequest(true)
    CompletableFuture.completedFuture(caps)
  
  override def configurationDone(args: ConfigurationDoneArguments): CompletableFuture[Void] =
    sendOutput("[DebugAdapter] Configuration done, starting trace...")
    CompletableFuture.completedFuture(null)
  
  override def launch(args: java.util.Map[String, AnyRef]): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def attach(args: java.util.Map[String, AnyRef]): CompletableFuture[Void] =
    sendOutput("[DebugAdapter] Attached, ready to trace")
    CompletableFuture.completedFuture(null)
  
  override def disconnect(args: DisconnectArguments): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def setBreakpoints(args: SetBreakpointsArguments): CompletableFuture[SetBreakpointsResponse] =
    CompletableFuture.completedFuture(SetBreakpointsResponse())
  
  override def setFunctionBreakpoints(args: SetFunctionBreakpointsArguments): CompletableFuture[SetFunctionBreakpointsResponse] =
    CompletableFuture.completedFuture(SetFunctionBreakpointsResponse())
  
  override def setExceptionBreakpoints(args: SetExceptionBreakpointsArguments): CompletableFuture[SetExceptionBreakpointsResponse] =
    CompletableFuture.completedFuture(SetExceptionBreakpointsResponse())
  
  override def continue_(args: ContinueArguments): CompletableFuture[ContinueResponse] =
    CompletableFuture.completedFuture(ContinueResponse())
  
  override def next(args: NextArguments): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def stepIn(args: StepInArguments): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def stepOut(args: StepOutArguments): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def pause(args: PauseArguments): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)
  
  override def stackTrace(args: StackTraceArguments): CompletableFuture[StackTraceResponse] =
    CompletableFuture.completedFuture(StackTraceResponse())
  
  override def scopes(args: ScopesArguments): CompletableFuture[ScopesResponse] =
    CompletableFuture.completedFuture(ScopesResponse())
  
  override def variables(args: VariablesArguments): CompletableFuture[VariablesResponse] =
    CompletableFuture.completedFuture(VariablesResponse())
  
  override def source(args: SourceArguments): CompletableFuture[SourceResponse] =
    CompletableFuture.completedFuture(SourceResponse())
  
  override def threads(): CompletableFuture[ThreadsResponse] =
    val response = ThreadsResponse()
    val thread = org.eclipse.lsp4j.debug.Thread()
    thread.setId(1)
    thread.setName("main")
    response.setThreads(Array(thread))
    CompletableFuture.completedFuture(response)
  
  override def evaluate(args: EvaluateArguments): CompletableFuture[EvaluateResponse] =
    CompletableFuture.completedFuture(EvaluateResponse())
  
  def close(): Unit =
    val socket = clientSocket
    if socket != null then socket.close()
    serverSocket.close()
    println(s"[DebugAdapter] Closed")
end DebugAdapter
