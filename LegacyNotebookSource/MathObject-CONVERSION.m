(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: MathObject *)
(* :Context: MathObject` *)
(* :Summary: Object oriented programming infrastructure *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright 2011, Mark A. Caprio *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 8.0 *)
(* :History: May 2011. Initiated.*)


(* ::Input::Initialization:: *)
(* :Discussion: To use the package, the contexts "MathObject`", "MathObject`Methods`", and "MathObject`InstanceData`" should all be in the $ContextPath.  

For interactive use: It always suffices to evaluate Get["MathObject`"], though this will cause the package to be reloaded unnecessarily if already loaded.  It normally suffices to evaluate Needs["MathObject`"], but this will fail to add the subcontexts to the $ContextPath if the package has already been loaded, say, privately to another package.  The completely proper way to load the package is through 

	Needs["MathObject`"];Needs[""MathObject`Methods`"];Needs["MathObject`InstanceData`"];

For internal use by a package: These Needs calls should be evaluated after BeginPackage.

For both internal use by a package *and* for subsequent external use outside the package: It is necessary to evaluate

BeginPackage["...",{MathObject`","MathObject`Methods`","MathObject`InstanceData`",...}]

Major limitation: Method names are in globally accessible namespace and therefore can easily be hidden, e.g., by a local symbol of the same name within a Module.  For example:

Module[
{x},
x=27;
Object@x[]; (* FAILS *)
];
*)


(* ::Section::Initialization:: *)
(*Begin package*)


(* ::Subsection::Initialization:: *)
(*Package context definition*)


(* ::Input::Initialization:: *)
BeginPackage[
"MathObject`",
{"MathObject`Methods`","MathObject`InstanceData`"}
];


(* ::Input::Initialization:: *)
Unprotect[Evaluate[$Context<>"*"]];


(* ::Subsection::Initialization:: *)
(*Usage messages*)


(* ::Input::Initialization:: *)
Object::usage="Object[name] signifies an object of the given name.";
DeclareClass::usage="DeclareClass[class,[parent,]{data1,data2,...},{method1,method2,...}] declares a class of objects, with given members.";
Destroy::usage="Destroy[object] destroys object.  Destruction is carried out by executing a generic destruction function, which in turn invokes the user-defined destructor as one step in the process.";
SetObjectData::usage="SetObjectData[o2,o1] populates all data fields in o2 with the values assigned for o1.  The object o1 must be of the same class as o2 or a daughter class, so insure that all these fields are present.";
ObjectExistsQ::usage="ObjectExistsQ[object] returns True if object is defined, i.e., has been created and not subsequently destroyed.";
ObjectClass::usage="ObjectClass[object] returns the class type of object.";
ObjectName::usage="ObjectName[object] returns the name of object.  (This is an expression, not necessarily a string.)";
ShowObject::usage="ShowObject[object] displays diagnostic information for object.";
ClassAncestry::usage="ClassAncestry[class] returns a list {...,grandparent,parent,class} of the class and its ancestors.";
ClassPattern::usage="ClassPattern[class] matches the class name of the given class or any descended class.";
ObjectPattern::usage="ObjectPattern[class] matches any object of the given class or any descended class.";
ObjectNamePattern::usage="ObjectNamePattern[class] matches the *name* of any object of the given class or any descended class.";
ScopeObjects::usage="ScopeObjects[body] evaluates body, and afterwards destroys all new objects remaining from the evaluation of body.";
ClearObjects::usage="ClearObjects[] removes all object instance data and clears the list of registered objects, without calling destructors.";
ConstructorWrapper::usage="MathObject internal implementation function.  External access is only needed in special circumstances for defining nonstandard syntaxes for calling the constructor.";


(* ::Input::Initialization:: *)
$ObjectClass::usage="Global storage for Object (not intended for direct access by user but visible for diagnostic purposes).";
$ObjectInstanceIdentifier::usage="Global storage for Object (not intended for direct access by user but visible for diagnostic purposes).";
$ObjectReference::usage="Global storage for Object (not intended for direct access by user but visible for diagnostic purposes).";
$ObjectRegistry::usage="Global registry of created objects (not intended for direct access by user but visible for diagnostic purposes).";


(* ::Subsection::Initialization:: *)
(*Global data*)


(* ::Input::Initialization:: *)
$ObjectMethodContext="MathObject`Methods`";
$ObjectInstanceContext="MathObject`InstanceData`";


(* ::Input::Initialization:: *)
$ObjectRegistry={};


(* ::Subsection::Initialization:: *)
(*Begin private context*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Subsection::Initialization:: *)
(*Dependencies*)


(* ::Input::Initialization:: *)



(* ::Section::Initialization:: *)
(*Error message utility*)


(* ::Subsection::Initialization:: *)
(*Conversion of argument list to string*)


(* ::Input::Initialization:: *)
ListToString[l_List]:=StringJoin@@Riffle[ToString/@l,","];


(* ::Section::Initialization:: *)
(*Class definition*)


(* ::Subsection::Initialization:: *)
(*Class declaration function*)


(* ::Text::Initialization:: *)
(*Declaration for class serves to:*)
(*	Define the special constructor access syntax for the class.*)
(*	Define mutator and accessor methods (SetXXXX[] and GetXXXX[]) for all the data member, declared so that they appear in the present context.*)
(*	Declare all other method symbols, so that they appear in the present context.*)
(*	Record the full set of method symbols, so that method calls can later be validated against them.*)
(*	Record the data member and method names in string form, for possible later inheritance by other classes.*)
(*	Record the parent class (or None), just in case that information is ever useful (it is not presently).*)
(*	Record clobbering permission for this class.*)
(*Class data:*)
(*	ClassDataMemberNames[class] -- list of data member name strings (needed for inheritance)*)
(*	ClassMethodNames[class] -- list of the method name strings (needed for inheritance)*)
(*		These are only the names given explicitly as the argument of DeclareClass, therefore excluding mutators/accessors (and the constructor/destuctor)*)
(*	ClassMethods[class] -- list of method symbols allowed for valid method calls (for method call validation)*)
(*		These therefore include both the explicitly named methods and the autogenerated mutators/accessors.  *)
(*		However, the list excludes the constructor/destuctor, since these are invoked with a special syntax rather than the usual method call syntax.*)
(*	ClassParent[class] -- parent class name or None (recorded for possible future use, though not presently needed)*)
(*	ClassAllowClobber[class] -- if an attempt is made to construct a new object (of any class) with the same name as a member of the present class, whether the constructor should be allowed to destroy (clobber) the existing object of the present class, or else fail with a call to the creation failure hook*)


(* ::Input::Initialization:: *)
Options[DeclareClass]={Replace->False};


(* ::Input::Initialization:: *)
DeclareClass[Class_Symbol,Parent_Symbol:None,DataMemberNamesP:{___String},MethodNamesP:{___String},OptionsPattern[]]:=Module[
{s,DataMemberNames,MethodNames,MutatorAccessorMethods,ExplicitMethods},

(* inherit data member and method names if applicable *)
DataMemberNames=Join[
DataMemberNamesP,
If[Parent===None,{},ClassDataMemberNames[Parent]]
];
MethodNames=Join[
MethodNamesP,
If[Parent===None,{},ClassMethodNames[Parent]]
];

(* constructor invocation syntax *)
(* no name -- Class[args] *)
Class/:HoldPattern[Class[Args___]]:=ConstructorWrapper[Class,Object[None]][Args];
(* name -- Class[[name]][args], or case of no name accepted as Class[[]][args] *)
(* Note: Intermediate reference to ConstructorWrapper[] without [Args] necessary sinceis since the definition
Class/:HoldPattern[Class[[n_:None]][Args___]]:=ConstructorWrapper[Class,Object[n]][Args];
places Class at too deep a level to be matched. *)
Class/:HoldPattern[Class[[n_:None]]]:=ConstructorWrapper[Class,Object[n]];

(* define data member mutators and accessors *)
MutatorAccessorMethods=Table[
With[
{
SetMethod=ToExpression[$ObjectMethodContext<>"Set"<>s],
GetMethod=ToExpression[$ObjectMethodContext<>"Get"<>s],
MemberIdentifier=ToExpression[$ObjectInstanceContext<>ToString[Class]<>"$"<>s]
},

SetMethod[Class,Self_Object][Value_]:=($ObjectInstanceIdentifier[Self][MemberIdentifier]=Value;Null);
GetMethod[Class,Self_Object][]:=($ObjectInstanceIdentifier[Self][MemberIdentifier]);
{SetMethod,GetMethod}
],
{s,DataMemberNames}
];

(* define symbols for explicit methods *)
ExplicitMethods=Table[
ToExpression[$ObjectMethodContext<>s],
{s,MethodNames}
];

(* save data member and method lists *)
ClassDataMemberNames[Class]=DataMemberNames;
ClassMethodNames[Class]=MethodNames;
ClassMethods[Class]=Flatten[{MutatorAccessorMethods,ExplicitMethods}];
ClassParent[Class]=Parent;

(* record clobbering permission for this class *)
ClassAllowClobber[Class]=OptionValue[Replace];

(* return Null *)
Null
];


(* ::Section::Initialization:: *)
(*Method access*)


(* ::Subsection::Initialization:: *)
(*Constructor wrapper function*)


(* ::Text::Initialization:: *)
(*Tasks:*)
(*	Checks for existing object of same name -- if present, clobbers or fails as appropriate.*)
(*	Registers object for scoping.*)
(*	Sets object metadata (identifier).*)
(*	Calls Constructor hook.*)
(*	If Constructor hook fails to evaluate (i.e., wrong argument sequence):*)
(*		Displays error message.*)
(*		Calls destructor wrapper to remove metadata. (CAVEAT: User destructor should therefore not assume constructor has run.)*)
(*		Calls hook function OnCreationFailure[Class,Self][], which, e.g., might be defined to throw an Abort[].*)
(*		Returns $Failed.*)
(*Return value is Self (or $Failed).*)


(* ::Input::Initialization:: *)
General::objdupl="Cannot create object `1`[[`2`]], since an object named \"`2`\" already exists (as an instance of class `3`).";
General::objsyntax="Missing or unexpected arguments in `1``2`[`3`].  (The given arguments do not match any of the definitions for the constructor for class `1`.)";


(* ::Input::Initialization:: *)
UniqueObjectBaseName=$ObjectInstanceContext<>"Object$";


(* ::Input::Initialization:: *)
ConstructorWrapper[Class_Symbol,AlmostSelf:Object[AlmostName_]][Args___]:=Module[
{Self,Name,NameArgumentString,Result,Aborted},

(* obtain object name if given as None *)
Name=If[
AlmostName===None,
Unique[UniqueObjectBaseName],
AlmostName
];
Self=Object[Name];

(* check for duplicate creation *)
If[
ObjectExistsQ[Self],
If[
ClassAllowClobber[ObjectClass[Self]],

(* if clobbering is allowed for class of *previously-existing* instance, destroy existing instance *)
Destroy[Self],

(* else fail at creation *)
Message[Class::objdupl,Class,Name,ObjectClass[Self]];
OnCreationFailure[Class,Self][Args];
Return[$Failed]

]
];

(* define instance metadata *)
$ObjectClass[Self]=Class;
$ObjectInstanceIdentifier[Self]=Unique[$ObjectInstanceContext<>"Instance$"];
$ObjectReference[$ObjectInstanceIdentifier[Self]]=Self;

(* do constructor body *)
(* evaluate body *)
Aborted=False;
CheckAbort[
Result=Constructor[Class,Self][Args],
Aborted=True
];

(* check for creation failure due to no constructor match *)
If[
MatchQ[Result,Constructor[_,_][___]],
NameArgumentString=Switch[
AlmostName,
None,"",
_,StringJoin["\[LeftDoubleBracket]",ToString[AlmostName],"\[RightDoubleBracket]"]
];
Message[Class::objsyntax,Class,NameArgumentString,ListToString[{Args}]];
ClearObjectData[Class,Self];
OnCreationFailure[Class,Self][];
Return[$Failed]
];

(* check for creation failure due to abort in constructor *)
If[
Aborted,
ClearObjectData[Class,Self];
Abort[];
Return[$Aborted]
];

(* register object *)
$ObjectRegistry=Union[$ObjectRegistry,{Self}];

(* return object *)
Self
];


(* ::Subsection::Initialization:: *)
(*Instance deletion*)


(* ::Text::Initialization:: *)
(*For use both in cleanup for failed construction and in normal destruction*)


(* ::Input::Initialization:: *)
ClearObjectData[Class_Symbol,Self:Object[n_]]:=Module[
{},

(* clear member data *)
Clear[Evaluate[$ObjectInstanceIdentifier[Self]]];

(* clear metadata *)
$ObjectReference[$ObjectInstanceIdentifier[Self]]=.;
$ObjectInstanceIdentifier[Self]=.;
$ObjectClass[Self]=.;
];


(* ::Subsection::Initialization:: *)
(*Destructor wrapper function*)


(* ::Input::Initialization:: *)
(*General::objdestroy="Attempting to destroy object `1` when this object does not exist.";*)


(* ::Input::Initialization:: *)
Destroy[Self:Object[A_]]:=Module[
{Class},

(* check that object exists in order to be destroyed *)
If[
!ObjectExistsQ[Self],
(*Message[General::objdestroy,Self];*)
Return[]
];

(* identify class *)
Class=ObjectClass[Self];

(* do destructor body *)
(* Note: If no explicit Destructor is defined, this is simply a no-op. *)
Destructor[Class,Self][];

(* delete all object member data and metadata *)
ClearObjectData[Class,Self];

(* deregister object *)
$ObjectRegistry=Complement[$ObjectRegistry,{Self}];

(* return Null *)
Null
];


(* ::Subsection::Initialization:: *)
(*General method access syntax*)


(* ::Input::Initialization:: *)
Object/:HoldPattern[(Self:Object[A_])@((Method_Symbol)[Args___])]:=MethodWrapper[Self,Method,Args];


(* ::Subsection::Initialization:: *)
(*General method access wrapper*)


(* ::Input::Initialization:: *)
Object::objaccess="Cannot complete method call `2`@`3`[`4`] since object does not exist.";
General::objmethod="Cannot complete method call `2`@`3`[`4`] since no method named `3` is defined for class `1`.";
General::objmethodsyntax="Cannot complete method call `2`@`3`[`4`] since given arguments do not match any of the definitions for method `3`.";


(* ::Input::Initialization:: *)
MethodWrapper[Self:Object[A_],Method_Symbol,Args___]:=Module[
{Class,Result,Msg},

(* validate object *)
If[
!ObjectExistsQ[Self],
Message[Object::objaccess,None,Self,Method,ListToString[{Args}]];
Return[$Failed]
];
Class=ObjectClass[Self];

(* validate method name *)
If[
!MemberQ[ClassMethods[Class],Method],
With[{ClassSymbol=Class},
Message[ClassSymbol::objmethod,Class,Self,Method,ListToString[{Args}]]
]; (* Note: If use Class::objmember directly, message name fails to resolve properly. *)
Return[$Failed]
];

(* invoke method *)
Result=Method[Class,Self][Args];

(* DEBUG: Print[{Method,Context[Method],Class,Context[Class]}];Print[Result]; *)

(* verify method call was matched and evaluated *)
If[
MatchQ[Result,Method[_,_][___]],
With[{ClassSymbol=Class},
Message[ClassSymbol::objmethodsyntax,Class,Self,Method,ListToString[{Args}]]
];
Return[$Failed]
];

(* return result *)
Result
];


(* ::Section::Initialization:: *)
(*Object operators*)


(* ::Subsection::Initialization:: *)
(*Memberwise assignment*)


(* ::Input::Initialization:: *)
SetObjectData::ancestry="Source object `1` is not of the same class as `2` or of a descendent class thereof.";


(* ::Input::Initialization:: *)
SetObjectData[o2_Object,o1_Object]:=Module[
{},

(* check ancestry of source object *)
If[
!MatchQ[o1,ObjectPattern[ObjectClass[o2]]],
Message[SetObjectData::ancestry,o1,o2];
Return[]
];

(* do copy *)
Do[
With[
{
SetMethod=ToExpression[$ObjectMethodContext<>"Set"<>s],
GetMethod=ToExpression[$ObjectMethodContext<>"Get"<>s]
},

o2@SetMethod[o1@GetMethod[]]
],
{s,ClassDataMemberNames[ObjectClass[o2]]}
];
];


(* ::Section::Initialization:: *)
(*Object metadata functions*)


(* ::Subsection::Initialization:: *)
(*Existence function*)


(* ::Input::Initialization:: *)
ObjectExistsQ[Self:Object[A_]]:=(Head[$ObjectClass[Self]]=!=$ObjectClass);


(* ::Subsection::Initialization:: *)
(*Object name extraction*)


(* ::Input::Initialization:: *)
ObjectName[Self:Object[A_]]:=A;


(* ::Subsection::Initialization:: *)
(*Object type retrieval function*)


(* ::Input::Initialization:: *)
ObjectClass::noclass="Attempting to determine class of object `1` when this object does not exist.";


(* ::Input::Initialization:: *)
ObjectClass[Self_Object]:=Module[
{},

(* check that object created *)
If[
!ObjectExistsQ[Self],
Message[ObjectClass::noclass,Self];
Return[]
];

(* retrieve class *)
$ObjectClass[Self]
];


(* ::Subsection::Initialization:: *)
(*Object data dump function*)


(* ::Input::Initialization:: *)
ShowObject[Self_Object]:=Module[
{},

Print["Object name: ",ObjectName[Self]];
If[
!ObjectExistsQ[Self],
Print["Object not defined."];
Return[]
];

Print["  ","Instance identifier: ",$ObjectInstanceIdentifier[Self]];
Print["  ","Class: ",ObjectClass[Self]];
(*Definition[Evaluate[$ObjectInstanceIdentifier[Self]]]*)
Print["  ",InputForm[Replace[
DownValues[Evaluate[$ObjectInstanceIdentifier[Self]]],
{x_:>(x[[1,1,1]]->x[[2]])},
{1}
]]]
];


(* ::Subsection::Initialization:: *)
(*Class ancestry*)


(* ::Input::Initialization:: *)
ClassAncestry::noclass="Ancestry requested for undefined class `1`.";


(* ::Input::Initialization:: *)
ClassAncestry[Class_Symbol]/;MatchQ[ClassParent[Class],None]:={Class};
ClassAncestry[Class_Symbol]/;MatchQ[ClassParent[Class],Except[None,_Symbol]]:=Append[ClassAncestry[ClassParent[Class]],Class];
ClassAncestry[Class_Symbol]/;MatchQ[ClassParent[Class],Except[_Symbol]]:=(Message[ClassAncestry::noclass,Class];{});


(* ::Text::Initialization:: *)
(*Note: Short-circuit && prevents attempt to retrieve ancestry of a symbol which does not represent a defined class name.*)


(* ::Input::Initialization:: *)
ClassPattern[Class_Symbol]:=(_Symbol)?(MatchQ[ClassParent[#],_Symbol]&&MemberQ[ClassAncestry[#],Class]&);


(* ::Subsection::Initialization:: *)
(*Object and object name matching patterns*)


(* ::Input::Initialization:: *)
ObjectPattern[Class_Symbol]:=(_Object)?(ObjectExistsQ[#]&&MemberQ[ClassAncestry[ObjectClass[#]],Class]&);


(* ::Input::Initialization:: *)
ObjectNamePattern[Class_Symbol]:=_?(ObjectExistsQ[Object[#]]&&MemberQ[ClassAncestry[ObjectClass[Object[#]]],Class]&);


(* ::Text::Initialization:: *)
(*Note: An ostensible alternate pattern would use a named expression (s_Object) combined with Condition.  In this case, HoldPattern on RHS needed, or else the else conditios bleed over to the assignment, and any reference to ObjectPattern[a] remains unevaluated.  However, even so, this pattern is not suitable for use in argument lists (or Repeated lists and sequences) involving more than one appearance of the pattern, since the use of the same name s in both appearances means that the pattern will match only if all the objects are the *same* instance.*)


(* ::Program::Initialization:: *)
(*ObjectPattern[Class_Symbol] :=*)
(*  HoldPattern[*)
(*   ((s_Object) /; ObjectExistsQ[s]) /; MemberQ[ClassAncestry[ObjectClass[s]], Class]*)
(*   ];*)
(*ObjectNamePattern[Class_Symbol]:=*)
(*  HoldPattern[*)
(*((n_)/;ObjectExistsQ[Object[n]])/;MemberQ[ClassAncestry[ObjectClass[Object[n]]],Class]*)
(*];*)


(* ::Section::Initialization:: *)
(*Scoping*)


(* ::Subsection::Initialization:: *)
(*Scoping of object duration*)


(* ::Text::Initialization:: *)
(*Any objects created within ScopeObjects will be destroyed upon exiting ScopeObjects. *)


(* ::Input::Initialization:: *)
SetAttributes[ScopeObjects,HoldAll];


(* ::Input::Initialization:: *)
ScopeObjects::numargs="ScopeObjects must be called with exactly one argument.";
ScopeObjects[_,__]:=Message[ScopeObjects::numargs];


(* ::Input::Initialization:: *)
(*
ScopeObjects[Body_]:=Module[
{
$ObjectRegistry0,Self,
EvaluatedBody,Aborted
},

AbortProtect[

(* record prior object registry *)
$ObjectRegistry0=$ObjectRegistry;

(* evaluate body *)
Aborted=False;
CheckAbort[
EvaluatedBody=Body,
Aborted=True
];

(* destroy any new objects which still exist *)
(* DEBUG: Print["ScopeObject cleanup: ",$ObjectRegistry0,$ObjectRegistry,Complement[$ObjectRegistry,$ObjectRegistry0]];*)
Do[
Destroy[Self],
{Self,Complement[$ObjectRegistry,$ObjectRegistry0]}
];
];

(* return value *)
(* passes through abort, and also explicitly returns $Aborted in case Abort[] is suppressed *)
If[Aborted,Abort[];$Aborted,EvaluatedBody]
];
*)


(* ::Input::Initialization:: *)
ScopeObjects[Body_]:=Module[
{
$ObjectRegistry0,Self,
EvaluatedBody,Aborted
},

Internal`WithLocalSettings[
(* initialization code*)
(* record prior object registry *)
$ObjectRegistry0=$ObjectRegistry,

(* body code*)
Body,

(* cleanup code *)
(* destroy any new objects which still exist *)
(* DEBUG: Print["ScopeObject cleanup: ",$ObjectRegistry0,$ObjectRegistry,Complement[$ObjectRegistry,$ObjectRegistry0]];*)
Do[
Destroy[Self],
{Self,Complement[$ObjectRegistry,$ObjectRegistry0]}
];
]
];


(* ::Subsection::Initialization:: *)
(*Clearing*)


(* ::Input::Initialization:: *)
ClearObjects[]:=Module[
{},
$ObjectRegistry={};
Quiet[
Remove["MathObject`InstanceData`*"],
{Remove::rmnsm}
]
]


(* ::Section::Initialization:: *)
(*End package*)


(* ::Subsection::Initialization:: *)
(*Exit private context*)


(* ::Input::Initialization:: *)
End[];


(* ::Subsection::Initialization:: *)
(*Exit package context*)


(* ::Input::Initialization:: *)
Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];
EndPackage[];
