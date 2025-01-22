
# Stub class for MeTTaLog
import io
import sys
import subprocess
import tempfile

include_called = False

# uncomment these as appropriate (do not remove commented out ones)
#BASH_BEFORE = ["swipl", "-q", "-f"]
#BASH_AFTER = ["-t", "halt"]
#BASH_BEFORE = ["metta"]
BASH_BEFORE = ["mettalog"]
BASH_AFTER = []

class MeTTaLog:
    def format_output(self, label, content):
        """
        Formats output depending on its length after trimming.
        """
        content = content.strip()

        if '\n' in content or len(content) > 80:
            return f"\n{label}```\n{content}\n```\n"
        else:
            return f"\n{label}`{content}`\n"

    def exec(self, code):
        """
        Executes a block of SWI-Prolog code by writing it to a temporary file and invoking the SWI-Prolog interpreter.
        Captures both stdout and stderr.
        """
        with tempfile.NamedTemporaryFile(delete=False, suffix=".metta", mode="w") as temp_file:
            temp_file.write(code)
            temp_file_path = temp_file.name

        try:
            command = BASH_BEFORE + [temp_file_path] + BASH_AFTER
            logging.debug(f"Executing command: {' '.join(command)}")
            process = subprocess.Popen(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            stdout, stderr = process.communicate()

            codewas = self.format_output("Execute:", code)
            captured_output = self.format_output("Captured Output:", stdout) if stdout.strip() else ""
            captured_error = self.format_output("Captured Errors:", stderr) if stderr.strip() else ""

            if process.returncode != 0:
                return codewas + captured_error

            if include_called:
                return codewas + captured_output + captured_error
            return captured_output + captured_error
        except Exception as e:
            return self.format_output("Error: ", str(e))
        finally:
            try:
                "" # os.remove(temp_file_path)
            except Exception as e:
                logging.error(f"Failed to remove temporary file: {e}")

    def eval(self, expression):
        """
        Evaluates a SWI-Prolog expression by writing it to a temporary file and invoking the SWI-Prolog interpreter.
        Captures both stdout and stderr.
        """
        #query_code = f"query_result :- {expression}, write(query_result).\n:- query_result.\n"
        expression = expression.strip()

        if expression.startswith('!'):
            query_code = expression + "\n"
        else:
            query_code = "!" + expression + "\n"


        with tempfile.NamedTemporaryFile(delete=False, suffix=".metta", mode="w") as temp_file:
            temp_file.write(query_code)
            temp_file_path = temp_file.name

        try:
            command = BASH_BEFORE + [temp_file_path] + BASH_AFTER
            logging.debug(f"Executing command: {' '.join(command)}")
            process = subprocess.Popen(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            stdout, stderr = process.communicate()

            codewas = self.format_output("Eval:", query_code)
            captured_result = self.format_output("Result:", stdout.strip()) if stdout.strip() else ""
            captured_error = self.format_output("Captured stderr:", stderr) if stderr.strip() else ""

            if process.returncode != 0:
                return codewas + captured_error

            if include_called:
                return codewas + captured_result + captured_error
            return captured_result + captured_error
        except Exception as e:
            return self.format_output("Error: ", str(e))
        finally:
            try:
               "" # os.remove(temp_file_path)
            except Exception as e:
                logging.error(f"Failed to remove temporary file: {e}")

# the above will be removed once we have the real class  and the next line uncommented

# from mettalog import MeTTaLog

import logging
from mmpy_bot import Plugin, listen_to
from mmpy_bot import Bot, Settings
import os
import json
import traceback


# Configure logging
logging.basicConfig(level=logging.DEBUG)

my_mettalog = MeTTaLog()

class MeTTaBotReplPlugin(Plugin):
    """
    A plugin that logs all attributes of the Message object, evaluates commands,
    and handles edits by responding appropriately.
    """

    async def respond_or_edit(self, message, response, edited=False):
        """
        Responds to a message or edits the bot's previous response if applicable.
        """
        if response is not None:  # Only respond if there's something to say
            try:
                if edited:
                    # Find the bot's last reply in the thread/channel
                    last_reply = self.driver.posts.get_posts_for_channel(
                        message.channel_id, params={'order': 'desc', 'limit': 10}
                    )
                    last_bot_post = next(
                        (post for post in last_reply['order']
                         if last_reply['posts'][post]['user_id'] == self.driver.bot_id),
                        None
                    )
                    if last_bot_post and 'message' in last_reply['posts'][last_bot_post]:
                        # Edit the last bot's post
                        self.driver.posts.update_post(last_bot_post, {'message': response})

                    else:
                        # Send a new message as a reply in a thread
                        self.driver.posts.create_post({
                            'channel_id': message.channel_id,
                            'message': response,
                            'root_id': message.id
                        })
                else:
                    # Send a new message in the same thread if possible
                    self.driver.posts.create_post({
                        'channel_id': message.channel_id,
                        'message': response,
                        'root_id': message.root_id or message.id
                    })
            except Exception as e:
                logging.error(f"Failed to respond or edit message: {e}")

    @listen_to(".*", needs_mention=False, message_edited=True)
    async def log_and_respond_edited(self, message):
        """
        Handles edited messages, evaluating them and editing the bot's response if needed.
        """
        response, should_reply = self.evaluate_command(message.text)
        if should_reply:
            await self.respond_or_edit(message, f"Edited: {response}", edited=True)

    @listen_to(".*", needs_mention=False, message_edited=False)
    async def log_and_respond(self, message, edited=False):
        """
        Handles new messages, evaluates commands, and responds if applicable.
        """
        self.print_message_attributes(message)
        response, should_reply = self.evaluate_command(message.text.strip())
        if should_reply:
            await self.respond_or_edit(message, response, edited=edited)

    def print_message_attributes(self, message):
        """
        Logs all attributes of the Message object for debugging purposes.
        """
        print("Flattened Message Attributes:")
        print(json.dumps(message.__dict__, indent=4))

    def evaluate_command(self, command: str) -> (str, bool):
        """
        Enhanced evaluation function to handle commands starting with '!', '(' and code blocks enclosed in triple backticks with 'metta'.
        """
        if not command:
            return None, False

        command = command.strip()

        try:
            # Check for code block enclosed in triple backticks with keyword 'metta'
            if '```metta' in command:
                code_block = command.split('```metta')[1].strip().split('```')[0]
                results = my_mettalog.exec(code_block)
                return results, True

            # Evaluate expressions starting with backticks
            if '`!' in command:
                expression = command.split('`!')[1].strip().split('`')[0]                
                result = my_mettalog.eval(expression)
                return result, True

            if '`(' in command:
                expression = command.split('`(')[1].strip().split('`')[0]                
                result = my_mettalog.exec(expression)
                return result, True

            # Handle single-line commands starting with '!'
            if command.startswith('!'):
                expression = command.split('!')[1].strip().split('\n')[0]                
                result = my_mettalog.eval(expression)
                return result, True

            # Handle commands wrapped in parentheses
            if command.startswith('(') and command.endswith(')'):
                result = my_mettalog.exec(command)
                return result, True

        except Exception as e:
            return f"Error: {traceback.format_exc()}", True

        return None, False  # No valid command detected

if __name__ == "__main__":
    # Start the bot with the custom settings
    bot = Bot(
        settings=Settings(
            MATTERMOST_URL="chat.singularitynet.io",
            MATTERMOST_PORT=443,
            BOT_TOKEN=os.getenv("MM_BOT_TOKEN") or (lambda: (_ for _ in ()).throw(ValueError("Environment variable 'MM_BOT_TOKEN' is not set")))(),
            BOT_TEAM="chat",
            SSL_VERIFY=True  # Maybe temporarily disable SSL verification for debugging
        ),
        plugins=[MeTTaBotReplPlugin()],
    )
    bot.run()


