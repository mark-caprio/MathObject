(* ::Package:: *)

(* ::Section::Initialization:: *)
(*Header comments*)


(* ::Input::Initialization:: *)
(* :Title: Methods *)
(* :Context: MathObject`Methods` *)
(* :Summary: Subcontext definition *)
(* :Author: Mark A. Caprio, Department of Physics, University of Notre Dame *)
(* :Copyright: Copyright 2011, Mark A. Caprio *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 8.0 *)
(* :History: See main package file. *)


(* ::Section::Initialization:: *)
(*Context for method definitions*)


(* ::Text::Initialization:: *)
(*It is desirable to have a separate namespace for methods.  Otherwise: If classes are defined in *different* packages, and if each package Protects the symbols in its context, then, if these classes have a member name in common, a shadowing and/or protection conflict will arise.*)
(**)
(*This context *must* be visible at user level in $ContextPath for access to methods.*)
(**)
(*Note therefore that usage messages should *not* be defined for the methods in any context *before* he method is declared with DeclareClass, which will properly establish the symbol in the methods context.*)


(* ::Input::Initialization:: *)
BeginPackage["MathObject`Methods`"];
Constructor::usage="Constructor[class,self][args] is the hook function for user constructor actions in MathObject package objects.";
OnCreationFailure::usage="OnCreationFailure[class,self][args] is the hook function invoked when the constructor call is not matched when creating an instance of a MathObject package object.  Definition of such a function is optional.";
Destructor::usage="Destructor[class,self][] is the hook function for user destructor actions in MathObject package objects.  Definition of such a function is optional.";
EndPackage[];
