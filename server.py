from flask import Flask, request, jsonify
from flask_cors import CORS
import tempfile
import subprocess
import os
import json

app = Flask(__name__)
CORS(app)

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
PROLOG_MAIN = os.path.join(PROJECT_ROOT, "prolog", "main.pl")

@app.route("/analyze", methods=["POST"])
def analyze():
    data = request.get_json()

    if not data or "code" not in data:
        return jsonify({"error": "No code provided"}), 400

    code = data["code"]

    try:
        violations = run_prolog(code)
        return jsonify(violations)

    except Exception as e:
        return jsonify({"error": str(e)}), 500


def run_prolog(code):
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".py", delete=False
    ) as f:
        f.write(code)
        python_file = f.name

    python_file = python_file.replace("\\", "/") # coz prolog can only read with /

    try:
        cmd = ["swipl", "-q", "-s", PROLOG_MAIN, "-g", f"analyze_and_print_json('{python_file}')", "-t", "halt"]

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=PROJECT_ROOT,
            timeout=30
        )

        if result.returncode != 0:
            raise RuntimeError(result.stderr)

        output = result.stdout.strip()
        return json.loads(output) if output else []

    finally:
        os.remove(python_file.replace("/", "\\")) # coz os can only read when \\


if __name__ == "__main__":
    app.run(port=5000, debug=True)
