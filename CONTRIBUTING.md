# Contributing to Pact Erlang

Thank you for your interest in contributing to Pact Erlang! This document provides guidelines and information for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How to Contribute](#how-to-contribute)
- [Development Setup](#development-setup)
- [Development Workflow](#development-workflow)
- [Testing](#testing)
- [Code Style and Standards](#code-style-and-standards)
- [Pull Request Process](#pull-request-process)
- [Release Process](#release-process)
- [Getting Help](#getting-help)

## Code of Conduct

This project adheres to a code of conduct that we expect all contributors to follow. Please be respectful and considerate in all interactions.

## How to Contribute

### Reporting Issues

Before creating an issue, please:

1. **Search existing issues** to avoid duplicates
2. **Use the latest version** to ensure the issue hasn't been fixed
3. **Provide detailed information** including:
   - Erlang/OTP version
   - Operating system
   - Steps to reproduce
   - Expected vs actual behavior
   - Relevant logs or error messages

### Suggesting Enhancements

Enhancement suggestions are welcome! Please:

1. **Check if the enhancement already exists** in issues or discussions
2. **Describe the use case** and why it would be valuable
3. **Provide examples** of how the feature would be used
4. **Consider backward compatibility** implications

### Contributing Code

We welcome code contributions! Types of contributions include:

- Bug fixes
- New features
- Performance improvements
- Documentation improvements
- Test coverage improvements
- Examples and tutorials

## Development Setup

### Prerequisites

- **Erlang/OTP**: Version 25, 26, 27, or 28
- **Rebar3**: Version 3.24.0 or later
- **Git**: For version control
- **Make**: For build automation
- **C compiler**: GCC or Clang (for compiling NIFs)
- **curl**: For downloading Pact FFI library

### Platform Support

- **Linux**: x86_64, aarch64
- **macOS**: x86_64, arm64 (Apple Silicon)

### Initial Setup

1. **Fork the repository** on GitHub
2. **Clone your fork**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/pact_erlang.git
   cd pact_erlang
   ```

3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/greyorange-labs/pact_erlang.git
   ```

4. **Install dependencies and compile**:
   ```bash
   make compile
   ```

### Project Structure

```
pact_erlang/
├── src/                         # Erlang source files
├── c_src/                       # C NIF source files
│   ├── include/                 # C header files
│   ├── pactffi_nif.c            # Main NIF implementation
│   ├── pact_verifier_external.c # Standalone verifier executable
│   └── Makefile                 # C build configuration
├── test/                        # Test suites
    ├── shell_scripts/           # test scripts
├── priv/                        # Private files (compiled artifacts)
├── _build/                      # Rebar3 build artifacts
├── CHANGELOG.md
├── CONTRIBUTING.md
├── README.md
├── LICENSE
├── Makefile
├── rebar.config
├── rebar.lock
├── codecov.json
└── ct-docker-compose.yml
```

## Development Workflow

### Branching Strategy

- **Main branch**: `develop` (default branch for development)
- **Feature branches**: `feature/your-feature-name`
- **Bug fix branches**: `fix/issue-description`

### Making Changes

1. **Create a feature branch**:
   ```bash
   git checkout develop
   git pull upstream develop
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the coding standards
3. **Add tests** for new functionality
4. **Update documentation** as needed
5. **Run the test suite** to ensure everything works
6. **Commit your changes** with clear messages

### Commit Message Format

Use clear, descriptive commit messages:

```
type(scope): short description

Longer description if needed, explaining what and why.

Fixes #123
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `build`: Build system changes
- `ci`: CI/CD changes

## Testing

### Running Tests

```bash
# Run all tests
make test

# Run tests with verbose output
rebar3 ct -v -c

# Run specific test suite
rebar3 ct --suite test/http_end_to_end_SUITE

# Generate coverage report
rebar3 cover -v
```

### Test Types

1. **Unit Tests**: Test individual modules and functions
2. **Integration Tests**: Test interaction between components
3. **End-to-End Tests**: Test complete workflows
4. **Message Pact Tests**: Test asynchronous messaging contracts
5. **HTTP Pact Tests**: Test HTTP API contracts

### Writing Tests

- **Follow existing patterns** in the test directory
- **Test both success and failure cases**
- **Use descriptive test names** that explain what is being tested
- **Include setup and cleanup** as needed
- **Add comments** for complex test logic

### Test Environment

Some tests require external services (like Pact Broker). Use the provided Docker Compose setup:

```bash
# Start test services
docker-compose -f ct-docker-compose.yml up -d

# Run tests
make test

# Stop services
docker-compose -f ct-docker-compose.yml down
```

## Code Style and Standards

### Erlang Code Style

This project uses `erlfmt` for consistent formatting:

```bash
# Check formatting
make check-format

# Auto-format code
make format
```

**Key Style Guidelines:**

- Use 4 spaces for indentation
- Line length limit: 120 characters
- Use descriptive variable names
- Add type specs for exported functions
- Include documentation for public APIs
- Follow OTP design principles

### C Code Style

For C NIFs:

- Use consistent indentation (4 spaces)
- Follow K&R style guidelines
- Include proper error handling
- Add comments for complex logic
- Use meaningful variable names

### Documentation

- **Public APIs**: Must have comprehensive documentation
- **Complex functions**: Should have inline comments
- **Examples**: Include usage examples in documentation
- **README updates**: Update README.md when adding major features

## Pull Request Process

### Before Submitting

1. **Ensure all tests pass**:
   ```bash
   make test
   ```

2. **Run static analysis**:
   ```bash
   make checks
   ```

3. **Update documentation** if needed
4. **Add changelog entry** if applicable

### Submitting the PR

1. **Push your branch** to your fork
2. **Create a Pull Request** against the `develop` branch
3. **Fill out the PR template** completely
4. **Link related issues** using keywords (e.g., "Fixes #123")

### PR Requirements

- ✅ All CI checks must pass
- ✅ Code coverage should not decrease
- ✅ All conversations must be resolved
- ✅ At least one maintainer approval required
- ✅ Up-to-date with target branch

### Review Process

1. **Automated checks** run on every PR
2. **Maintainer review** for code quality and design
3. **Testing** on multiple platforms
4. **Documentation review** if applicable
5. **Final approval** and merge

## Release Process

### Version Management

This project uses [Semantic Versioning](https://semver.org/):

- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Steps

1. Update version in `src/pact_erlang.app.src`
2. Update `CHANGELOG.md` with release notes
3. Commit changes and create a git tag
4. Push tag to trigger release workflow
5. Publish to Hex.pm

Example release checklist from README:
- Update version in `src/pact_erlang.app.src`
- Update CHANGELOG.md
- Run `rebar3 hex publish --dry-run`
- Commit files and add git tag
- Push to remote
- Run `rebar3 hex publish`

## Static Analysis and Quality Checks

The project includes several quality assurance tools:

```bash
# Run all checks
make checks

# Individual checks
make xref          # Cross-reference analysis
make dialyzer      # Static type analysis
make check-format  # Code formatting check
```

### Dialyzer

Dialyzer performs static analysis to find type errors:

```bash
rebar3 as dialyzer dialyzer
```

Add type specifications to help Dialyzer:

```erlang
-spec function_name(Arg1Type, Arg2Type) -> ReturnType.
```

### Xref

Cross-reference analysis finds unused functions and calls to undefined functions:

```bash
rebar3 xref
```

## Platform-Specific Notes

### Linux Development

- Supports x86_64 and aarch64 architectures
- Uses system GCC compiler
- Builds standalone verifier executable

### macOS Development

- Supports both Intel and Apple Silicon
- Uses Xcode command line tools
- May require additional setup for C compilation

### Dependencies

The project automatically downloads required dependencies:

- **Pact FFI Library**: Downloaded during build process
- **Erlang Dependencies**: Managed by Rebar3
- **C Headers**: Included in the repository

## Troubleshooting

### Common Issues

1. **Compilation Failures**:
   - Ensure you have a C compiler installed
   - Check that Erlang development headers are available
   - Verify network access for downloading Pact FFI library

2. **Test Failures**:
   - Ensure all required services are running
   - Check for port conflicts
   - Verify test data is properly set up

3. **NIF Loading Issues**:
   - Ensure the shared library was compiled correctly
   - Check architecture compatibility
   - Verify library dependencies are available

### Getting Debug Information

```bash
# Verbose compilation
make compile V=1

# Detailed test output
rebar3 ct -v -c --readable true

# Check NIF loading
erl -eval "application:start(pact_erlang), init:stop()."
```

## Getting Help

### Community Resources

- **GitHub Issues**: For bug reports and feature requests
- **GitHub Discussions**: For questions and general discussion
- **Documentation**: Available at [hexdocs.pm/pact_erlang](https://hexdocs.pm/pact_erlang)

### Maintainer Contact

When reaching out:

1. **Search existing issues** first
2. **Provide context** about your environment and use case
3. **Include relevant code** or error messages
4. **Be patient** - maintainers contribute in their spare time

### Contributing to Documentation

Documentation improvements are always welcome:

- Fix typos or unclear explanations
- Add missing examples
- Improve API documentation
- Add tutorials or guides

Thank you for contributing to Pact Erlang! Your contributions help make contract testing more accessible to the Erlang community.
