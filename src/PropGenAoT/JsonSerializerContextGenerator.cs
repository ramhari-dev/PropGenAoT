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
        // Get all classes and records that implement IDto
        var dtoTypeDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
            .Where(static m => m is not null);

        // Combine with compilation
        var compilation = context.CompilationProvider.Combine(dtoTypeDeclarations.Collect());

        // Generate the source
        context.RegisterSourceOutput(compilation,
            static (spc, source) => Execute(source.Left, source.Right, spc));
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
    {
        return node is TypeDeclarationSyntax { BaseList: not null } typeDecl &&
               (typeDecl is ClassDeclarationSyntax || typeDecl is RecordDeclarationSyntax);
    }

    private static TypeDeclarationSyntax? GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
    {
        var typeDeclaration = (TypeDeclarationSyntax)context.Node;
        
        foreach (var baseType in typeDeclaration.BaseList?.Types ?? default)
        {
            var typeInfo = context.SemanticModel.GetTypeInfo(baseType.Type);
            var typeSymbol = typeInfo.Type;

            if (typeSymbol == null)
                continue;

            var containingNamespace = typeSymbol.ContainingNamespace?.ToDisplayString();
            var typeName = typeSymbol.Name;

            if (containingNamespace == "PropGenAoT.Abstractions" && typeName == "IDto")
            {
                return typeDeclaration;
            }
        }

        return null;
    }

    private static void Execute(Compilation compilation, ImmutableArray<TypeDeclarationSyntax?> types, SourceProductionContext context)
    {
        if (types.IsDefaultOrEmpty)
            return;

        var distinctTypes = types.Distinct().ToList();
        
        var sourceBuilder = new StringBuilder();
        sourceBuilder.AppendLine("using System.Text.Json;");
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
        foreach (var typeDeclaration in distinctTypes)
        {
            if (typeDeclaration == null)
                continue;
                
            var typeSymbol = compilation.GetSemanticModel(typeDeclaration.SyntaxTree)
                .GetDeclaredSymbol(typeDeclaration);
            
            if (typeSymbol == null)
                continue;

            var fullTypeName = typeSymbol.ToDisplayString();
            sourceBuilder.AppendLine($"[JsonSerializable(typeof({fullTypeName}))]");
            sourceBuilder.AppendLine($"[JsonSerializable(typeof({fullTypeName}[]))]");
        }

        sourceBuilder.AppendLine("internal partial class PropGenJsonContext : JsonSerializerContext");
        sourceBuilder.AppendLine("{");
        sourceBuilder.AppendLine("}");

        context.AddSource("PropGenJsonContext.g.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
    }

    private static string GetCompilationNamespace(Compilation compilation)
    {
        return compilation.AssemblyName ?? string.Empty;
    }
}