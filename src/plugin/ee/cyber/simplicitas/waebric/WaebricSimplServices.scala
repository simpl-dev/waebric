// This file contains generated code. You are not supposed to edit it.
// Instead, use the file WaebricSimplConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.waebric

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object WaebricSimplPlugin {
  var instance: WaebricSimplPlugin = null
  
  def getInstance =
    if (instance eq null)
      new WaebricSimplPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class WaebricSimplPlugin extends SimplicitasPlugin {
  def getID = WaebricSimplConfig.pluginId
  def getLanguageID = WaebricSimplConfig.languageId

  WaebricSimplPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    WaebricSimplConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            WaebricSimplConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    WaebricSimplConfig.initializeImages(addFun)
  }
}

class WaebricSimplParseController extends 
  SimplicitasParseController(WaebricSimplConfig.language, WaebricSimplConfig.instance) {}

class WaebricSimplTokenColorer extends TokenColorerBase(WaebricSimplPlugin.factory,
                                                    WaebricSimplConfig.instance) {}

class WaebricSimplTreeModelBuilder
  extends SimplicitasTreeModelBuilder(WaebricSimplConfig.instance) {
}

class WaebricSimplLabelProvider
    extends SimplicitasLabelProvider(WaebricSimplConfig.instance) {}

class WaebricSimplReferenceResolver
    extends SimplicitasReferenceResolver(WaebricSimplConfig.instance) {}

class WaebricSimplFoldingUpdater
    extends SimplicitasFoldingUpdater(WaebricSimplConfig.instance) {}

class WaebricSimplDocumentationProvider
    extends SimplicitasDocumentationProvider(WaebricSimplConfig.instance) {}

class WaebricSimplContentProposer
    extends SimplicitasContentProposer(WaebricSimplConfig.instance) {}

class WaebricSimplOccurrenceMarker
    extends SimplicitasOccurrenceMarker(WaebricSimplConfig.instance) {}

class WaebricSimplPreferencePage
    extends SimplicitasPreferencePage(WaebricSimplPlugin.factory) {}

class WaebricSimplPreferenceInitializer
    extends SimplicitasPreferenceInitializer(WaebricSimplPlugin.factory) {}

class WaebricSimplGenerateAction extends GenerateAction(WaebricSimplConfig.instance) {}