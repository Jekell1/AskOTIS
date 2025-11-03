// Updated question generation function with difficulty levels
// Replace the generateRandomQuestions function in otis-rag-chat.html with this

function generateRandomQuestions(count = 500) {
    const questions = [];
    
    // Difficulty level indicators
    const levels = {
        easy: { emoji: '🟢', label: 'Easy' },
        medium: { emoji: '🟡', label: 'Medium' },
        hard: { emoji: '🟠', label: 'Hard' },
        veryHard: { emoji: '🔴', label: 'Expert' }
    };

    // EASY questions - Simple lookups (40%)
    const easyTemplates = [
        (p) => ({ q: `What does program ${p} do?`, level: 'easy' }),
        (c) => ({ q: `What programs use copybook ${c}?`, level: 'easy' }),
        () => ({ q: `What are the main menu options in LPMENU?`, level: 'easy' }),
        () => ({ q: `List all payment processing programs`, level: 'easy' }),
        (p) => ({ q: `What copybooks does ${p} use?`, level: 'easy' }),
        () => ({ q: `Show me programs that handle loans`, level: 'easy' }),
        (c) => ({ q: `Show me fields defined in ${c}`, level: 'easy' }),
        () => ({ q: `What screens are available in the system?`, level: 'easy' }),
        (p) => ({ q: `What files does ${p} access?`, level: 'easy' }),
        () => ({ q: `Find programs related to customer inquiries`, level: 'easy' })
    ];

    // MEDIUM questions - Relationships (35%)
    const mediumTemplates = [
        (p) => ({ q: `Explain how ${p} works and what it calls`, level: 'medium' }),
        (t) => ({ q: `Show me the workflow for ${t}`, level: 'medium' }),
        (p) => ({ q: `What programs call ${p} and why?`, level: 'medium' }),
        (t) => ({ q: `How does the system process ${t}?`, level: 'medium' }),
        () => ({ q: `Walk me through the daily processing cycle`, level: 'medium' }),
        (c) => ({ q: `Explain the data structure in ${c} and how it's used`, level: 'medium' }),
        () => ({ q: `Show me the screen flow for payment processing`, level: 'medium' }),
        () => ({ q: `What are the business rules for interest calculation?`, level: 'medium' }),
        (p) => ({ q: `Trace the execution flow when ${p} is called`, level: 'medium' }),
        () => ({ q: `How are late fees calculated and applied?`, level: 'medium' })
    ];

    // HARD questions - Cross-index analysis (20%)
    const hardTemplates = [
        (p) => ({ q: `Show all programs, copybooks, and data items in the ${p} call chain`, level: 'hard' }),
        (c) => ({ q: `If I modify ${c}, what is the complete blast radius?`, level: 'hard' }),
        () => ({ q: `Trace customer balance data lineage from file read to write`, level: 'hard' }),
        () => ({ q: `Find all circular dependencies between programs`, level: 'hard' }),
        (t) => ({ q: `Map complete end-to-end flow for ${t} with screens and files`, level: 'hard' }),
        () => ({ q: `What are all navigation paths to Customer Inquiry screen?`, level: 'hard' }),
        () => ({ q: `Show me programs never called by any other program`, level: 'hard' }),
        (p) => ({ q: `Complete dependency tree for ${p} including transitive deps`, level: 'hard' }),
        () => ({ q: `Identify programs using different SCREEN copybook versions`, level: 'hard' })
    ];

    // VERY HARD questions - Complex analysis (5%)
    const veryHardTemplates = [
        () => ({ q: `Analyze top 5 complexity hotspots and recommend refactoring`, level: 'veryHard' }),
        (t) => ({ q: `Where should I add a new ${t} feature? Provide implementation plan`, level: 'veryHard' }),
        () => ({ q: `Find data consistency risks from copybook conflicts and suggest fixes`, level: 'veryHard' }),
        () => ({ q: `Map transaction LPMT to complete technical implementation`, level: 'veryHard' }),
        () => ({ q: `Design test strategy for payment subsystem based on code analysis`, level: 'veryHard' }),
        () => ({ q: `Identify architectural anti-patterns and propose modernization`, level: 'veryHard' }),
        () => ({ q: `Analyze error handling patterns and recommend improvements`, level: 'veryHard' })
    ];

    // Calculate counts per difficulty
    const easyCount = Math.floor(count * 0.40);
    const mediumCount = Math.floor(count * 0.35);
    const hardCount = Math.floor(count * 0.20);
    const veryHardCount = count - easyCount - mediumCount - hardCount;

    // Helper to generate from templates
    function generateFromTemplates(templates, targetCount) {
        const generated = [];
        while (generated.length < targetCount) {
            const template = templates[Math.floor(Math.random() * templates.length)];
            let result;

            if (template.length === 1) {
                const rand = Math.random();
                if (rand < 0.4 && questionTemplates.programs.length > 0) {
                    const prog = questionTemplates.programs[Math.floor(Math.random() * questionTemplates.programs.length)];
                    result = template(prog);
                } else if (rand < 0.7 && questionTemplates.copybooks.length > 0) {
                    const copy = questionTemplates.copybooks[Math.floor(Math.random() * questionTemplates.copybooks.length)];
                    result = template(copy);
                } else if (questionTemplates.topics.length > 0) {
                    const topic = questionTemplates.topics[Math.floor(Math.random() * questionTemplates.topics.length)];
                    result = template(topic);
                }
            } else {
                result = template();
            }

            if (result) {
                const levelInfo = levels[result.level];
                const fullQuestion = `${levelInfo.emoji} ${result.q}`;
                if (!questions.some(q => q.text === fullQuestion)) {
                    generated.push({ text: fullQuestion, level: result.level });
                }
            }
        }
        return generated;
    }

    // Generate for each difficulty level
    questions.push(...generateFromTemplates(easyTemplates, easyCount));
    questions.push(...generateFromTemplates(mediumTemplates, mediumCount));
    questions.push(...generateFromTemplates(hardTemplates, hardCount));
    questions.push(...generateFromTemplates(veryHardTemplates, veryHardCount));

    return questions;
}
