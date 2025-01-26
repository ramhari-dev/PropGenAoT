using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System.Collections.Immutable;
using System.Text;

namespace PropGenAoT;


/// <summary>
/// A source generator that generates JSON serialization context for DTO classes.
/// </summary>
[Generator]
public class JsonSerializerContextGenerator : IIncrementalGenerator
{
    /// <summary>
    /// Initialize
    /// </summary>
    /// <param name="context"></param>
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Get all classes that implement IDto
        var dtoClassDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
            .Where(static m => m is not null);

        // Combine with compilation
        var compilation = context.CompilationProvider.Combine(dtoClassDeclarations.Collect());

        // Generate the source
        context.RegisterSourceOutput(compilation,
            static (spc, source) => Execute(source.Left, source.Right.Cast<ClassDeclarationSyntax>().ToImmutableArray(), spc));
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
    {
        return node is ClassDeclarationSyntax { BaseList: not null };
    }

    private static ClassDeclarationSyntax? GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
    {
        var classDeclaration = (ClassDeclarationSyntax)context.Node;
        
        foreach (var baseType in classDeclaration.BaseList?.Types ?? default)
        {
            var typeInfo = context.SemanticModel.GetTypeInfo(baseType.Type);
            var typeSymbol = typeInfo.Type;

            if (typeSymbol == null)
                continue;

            var containingNamespace = typeSymbol.ContainingNamespace?.ToDisplayString();
            var typeName = typeSymbol.Name;

            if (containingNamespace == "PropGenAoT.Abstractions" && typeName == "IDto")
            {
                return classDeclaration;
            }
        }

        return null;
    }

    private static void Execute(Compilation compilation, ImmutableArray<ClassDeclarationSyntax> classes, SourceProductionContext context)
    {
        if (classes.IsDefaultOrEmpty)
            return;

        var distinctClasses = classes.Distinct().ToList();
        
        var sourceBuilder = new StringBuilder();
        sourceBuilder.AppendLine("using System.Text.Json.Serialization;");
        sourceBuilder.AppendLine();
        
        var compilationNamespace = GetCompilationNamespace(compilation);
        if (!string.IsNullOrEmpty(compilationNamespace))
        {
            sourceBuilder.AppendLine($"namespace {compilationNamespace};");
            sourceBuilder.AppendLine();
        }

        sourceBuilder.AppendLine("[JsonSourceGenerationOptions(WriteIndented = true)]");
        
        // Generate JsonSerializable attributes for each DTO type
        foreach (var classDeclaration in distinctClasses)
        {
            var classSymbol = compilation.GetSemanticModel(classDeclaration.SyntaxTree)
                .GetDeclaredSymbol(classDeclaration);
            
            if (classSymbol == null)
                continue;

            var fullTypeName = classSymbol.ToDisplayString();
            sourceBuilder.AppendLine($"[JsonSerializable(typeof({fullTypeName}))]");
            sourceBuilder.AppendLine($"[JsonSerializable(typeof({fullTypeName}[]))]");
        }

        sourceBuilder.AppendLine("internal partial class PropGenJsonContext : JsonSerializerContext");
        sourceBuilder.AppendLine("{");
        sourceBuilder.AppendLine("}");

        context.AddSource("AppJsonSerializerContext.g.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
    }

    private static string GetCompilationNamespace(Compilation compilation)
    {
        // Try to get the default namespace from the compilation
        return compilation.AssemblyName ?? string.Empty;
    }
}