﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.1378
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using System.Xml.Serialization;

// 
// This source code was auto-generated by xsd, Version=2.0.50727.42.
// 


/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class JarAnalyzer {
    
    private Jar[] jarsField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlArrayItemAttribute("Jar", IsNullable=false)]
    public Jar[] Jars {
        get {
            return this.jarsField;
        }
        set {
            this.jarsField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class Jar {
    
    private Summary summaryField;
    
    private string[] textField;
    
    private string nameField;
    
    /// <remarks/>
    public Summary Summary {
        get {
            return this.summaryField;
        }
        set {
            this.summaryField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlTextAttribute()]
    public string[] Text {
        get {
            return this.textField;
        }
        set {
            this.textField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public string name {
        get {
            return this.nameField;
        }
        set {
            this.nameField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class Summary {
    
    private Statistics statisticsField;
    
    private Metrics metricsField;
    
    private string[] packagesField;
    
    private OutgoingDependencies outgoingDependenciesField;
    
    private IncomingDependencies incomingDependenciesField;
    
    private Cycles cyclesField;
    
    private UnresolvedDependencies unresolvedDependenciesField;
    
    /// <remarks/>
    public Statistics Statistics {
        get {
            return this.statisticsField;
        }
        set {
            this.statisticsField = value;
        }
    }
    
    /// <remarks/>
    public Metrics Metrics {
        get {
            return this.metricsField;
        }
        set {
            this.metricsField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlArrayItemAttribute("Package", IsNullable=false)]
    public string[] Packages {
        get {
            return this.packagesField;
        }
        set {
            this.packagesField = value;
        }
    }
    
    /// <remarks/>
    public OutgoingDependencies OutgoingDependencies {
        get {
            return this.outgoingDependenciesField;
        }
        set {
            this.outgoingDependenciesField = value;
        }
    }
    
    /// <remarks/>
    public IncomingDependencies IncomingDependencies {
        get {
            return this.incomingDependenciesField;
        }
        set {
            this.incomingDependenciesField = value;
        }
    }
    
    /// <remarks/>
    public Cycles Cycles {
        get {
            return this.cyclesField;
        }
        set {
            this.cyclesField = value;
        }
    }
    
    /// <remarks/>
    public UnresolvedDependencies UnresolvedDependencies {
        get {
            return this.unresolvedDependenciesField;
        }
        set {
            this.unresolvedDependenciesField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class Statistics {
    
    private short classCountField;
    
    private short abstractClassCountField;
    
    private sbyte packageCountField;
    
    /// <remarks/>
    public short ClassCount {
        get {
            return this.classCountField;
        }
        set {
            this.classCountField = value;
        }
    }
    
    /// <remarks/>
    public short AbstractClassCount {
        get {
            return this.abstractClassCountField;
        }
        set {
            this.abstractClassCountField = value;
        }
    }
    
    /// <remarks/>
    public sbyte PackageCount {
        get {
            return this.packageCountField;
        }
        set {
            this.packageCountField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class Metrics {
    
    private decimal abstractnessField;
    
    private sbyte efferentField;
    
    private sbyte afferentField;
    
    private decimal instabilityField;
    
    private decimal distanceField;
    
    /// <remarks/>
    public decimal Abstractness {
        get {
            return this.abstractnessField;
        }
        set {
            this.abstractnessField = value;
        }
    }
    
    /// <remarks/>
    public sbyte Efferent {
        get {
            return this.efferentField;
        }
        set {
            this.efferentField = value;
        }
    }
    
    /// <remarks/>
    public sbyte Afferent {
        get {
            return this.afferentField;
        }
        set {
            this.afferentField = value;
        }
    }
    
    /// <remarks/>
    public decimal Instability {
        get {
            return this.instabilityField;
        }
        set {
            this.instabilityField = value;
        }
    }
    
    /// <remarks/>
    public decimal Distance {
        get {
            return this.distanceField;
        }
        set {
            this.distanceField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class OutgoingDependencies {
    
    private Jar[] jarField;
    
    private string[] textField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("Jar")]
    public Jar[] Jar {
        get {
            return this.jarField;
        }
        set {
            this.jarField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlTextAttribute()]
    public string[] Text {
        get {
            return this.textField;
        }
        set {
            this.textField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class IncomingDependencies {
    
    private Jar[] jarField;
    
    private string[] textField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("Jar")]
    public Jar[] Jar {
        get {
            return this.jarField;
        }
        set {
            this.jarField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlTextAttribute()]
    public string[] Text {
        get {
            return this.textField;
        }
        set {
            this.textField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class Cycles {
    
    private string[] cycleField;
    
    private string[] textField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("Cycle")]
    public string[] Cycle {
        get {
            return this.cycleField;
        }
        set {
            this.cycleField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlTextAttribute()]
    public string[] Text {
        get {
            return this.textField;
        }
        set {
            this.textField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.42")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class UnresolvedDependencies {
    
    private string[] packageField;
    
    private string[] textField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("Package")]
    public string[] Package {
        get {
            return this.packageField;
        }
        set {
            this.packageField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlTextAttribute()]
    public string[] Text {
        get {
            return this.textField;
        }
        set {
            this.textField = value;
        }
    }
}
