import ast
from collections import defaultdict, deque


class ClassDependencyAnalyzer(ast.NodeVisitor):
    def __init__(self):
        self.classes = set()
        self.dependencies = defaultdict(set)

    def visit_ClassDef(self, node):
        self.classes.add(node.name)
        for base in node.bases:
            if isinstance(base, ast.Name):
                self.dependencies[node.name].add(base.id)
        self.generic_visit(node)

    def visit_AnnAssign(self, node):
        if isinstance(node.annotation, ast.Name):
            self.dependencies[node.target.id].add(node.annotation.id)
        elif isinstance(node.annotation, ast.Subscript):
            if isinstance(node.annotation.value, ast.Name):
                self.dependencies[node.target.id].add(node.annotation.value.id)
        self.generic_visit(node)

    def visit_Attribute(self, node):
        if isinstance(node.value, ast.Name):
            self.dependencies[node.value.id].add(node.attr)
        self.generic_visit(node)

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load) and node.id in self.classes:
            self.dependencies[node.id].add(node.id)  # Avoid self-references
        self.generic_visit(node)

    def visit_Subscript(self, node):
        if isinstance(node.value, ast.Name):
            self.dependencies[node.value.id].add(node.value.id)
        self.generic_visit(node)

def collect_necessary_classes(target_classes, dependencies):
    necessary_classes = set()
    queue = deque(target_classes)

    while queue:
        cls = queue.popleft()
        if cls not in necessary_classes:
            necessary_classes.add(cls)
            for dep in dependencies[cls]:
                if dep not in necessary_classes:  # Only add if not already necessary
                    queue.append(dep)

    return necessary_classes

def filter_classes(file_path, target_classes):
    with open(file_path, 'r') as file:
        source = file.read()

    tree = ast.parse(source)
    analyzer = ClassDependencyAnalyzer()
    analyzer.visit(tree)

    necessary_classes = collect_necessary_classes(target_classes, analyzer.dependencies)

    # Separate imports and classes
    imports = []
    class_nodes = []

    for node in tree.body:
        if isinstance(node, ast.Import) or isinstance(node, ast.ImportFrom):
            imports.append(node)
        elif isinstance(node, ast.ClassDef) and node.name in necessary_classes:
            class_nodes.append(node)

    # Generate the new source code
    new_source = []

    # Add imports
    for imp in imports:
        new_source.append(ast.unparse(imp))

    # Add classes
    for cls in class_nodes:
        new_source.append(ast.unparse(cls))

    # Write back to the file
    with open(file_path, 'w') as file:
        file.write('\n\n'.join(new_source) + '\n')

if __name__ == "__main__":
    file_path = 'TS28541_SliceNrm.py'  # Update with your actual file path
    target_classes = [
        'ServiceProfile',
        'SliceProfile',
        'RANSliceSubnetProfile',
        'CNSliceSubnetProfile',
        'TopSliceSubnetProfile'
    ]  # Specify the classes you want to keep
    filter_classes(file_path, target_classes)
