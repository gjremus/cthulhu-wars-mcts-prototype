package cws

import java.io.{File, FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Diagnostic trace logger for debugging infinite loops.
 *  Appends timestamped messages to /tmp/arena_trace.log */
object TraceLog {
    private val file = new File("/tmp/arena_trace.log")
    private val fmt = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

    def log(msg: String): Unit = synchronized {
        val pw = new PrintWriter(new FileWriter(file, true))
        try {
            val ts = LocalDateTime.now().format(fmt)
            pw.println(s"$ts $msg")
            pw.flush()
        } finally {
            pw.close()
        }
    }

    def clear(): Unit = synchronized {
        val pw = new PrintWriter(new FileWriter(file, false))
        pw.close()
    }
}
